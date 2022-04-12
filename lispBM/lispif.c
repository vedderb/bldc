/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Joel Svensson    svenssonjoel@yahoo.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "lispif.h"
#include "commands.h"
#include "terminal.h"
#include "flash_helper.h"
#include "buffer.h"
#include "timeout.h"
#include "lispbm.h"

#define HEAP_SIZE				2048
#define LISP_MEM_SIZE			LBM_MEMORY_SIZE_8K
#define LISP_MEM_BITMAP_SIZE	LBM_MEMORY_BITMAP_SIZE_8K
#define GC_STACK_SIZE			160
#define PRINT_STACK_SIZE		128
#define EXTENSION_STORAGE_SIZE	180
#define VARIABLE_STORAGE_SIZE	128

__attribute__((section(".ram4"))) static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static uint32_t memory_array[LISP_MEM_SIZE];
static uint32_t bitmap_array[LISP_MEM_BITMAP_SIZE];
static uint32_t gc_stack_storage[GC_STACK_SIZE];
static uint32_t print_stack_storage[PRINT_STACK_SIZE];
static extension_fptr extension_storage[EXTENSION_STORAGE_SIZE];
static lbm_value variable_storage[VARIABLE_STORAGE_SIZE];

static lbm_tokenizer_string_state_t string_tok_state;
static lbm_tokenizer_char_stream_t string_tok;

static thread_t *eval_tp = 0;
static THD_FUNCTION(eval_thread, arg);
static THD_WORKING_AREA(eval_thread_wa, 2048);
static bool lisp_thd_running = false;

static int repl_cid = -1;

// Private functions
static bool start_lisp(bool print, bool load_code);
static uint32_t timestamp_callback(void);
static void sleep_callback(uint32_t us);

void lispif_init(void) {
	// Do not attempt to start lisp after a watchdog reset, in case lisp
	// was the cause of it.
	// TODO: Anything else to check?
	if (!timeout_had_IWDG_reset() && terminal_get_first_fault() != FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET) {
		start_lisp(false, true);
	}
}

static void ctx_cb(eval_context_t *ctx, void *arg1, void *arg2) {
	if (arg2 != NULL) {
		lbm_print_value((char*)arg2, 40, ctx->r);
	}

	if (arg1 != NULL) {
		float *res = (float*)arg1;
		*res = 100.0 * (float)ctx->K.max_sp / 256.0;
	}
}

static void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
	(void) arg1;
	(void) arg2;

	char output[128];

	int print_ret = lbm_print_value(output, sizeof(output), ctx->r);

	commands_printf_lisp("--------------------------------");
	commands_printf_lisp("ContextID: %u", ctx->id);
	commands_printf_lisp("Stack SP: %u",  ctx->K.sp);
	commands_printf_lisp("Stack SP max: %u", ctx->K.max_sp);
	if (print_ret) {
		commands_printf_lisp("Value: %s", output);
	} else {
		commands_printf_lisp("Error: %s", output);
	}
}

static void sym_it(const char *str) {
	commands_printf_lisp("%s", str);
}

void lispif_process_cmd(unsigned char *data, unsigned int len,
		void(*reply_func)(unsigned char *data, unsigned int len)) {
	COMM_PACKET_ID packet_id;

	packet_id = data[0];
	data++;
	len--;

	switch (packet_id) {
	case COMM_LISP_SET_RUNNING: {
		bool ok = false;
		bool running = data[0];
		lispif_disable_all_events();

		if (!running) {
			int timeout_cnt = 20;
			lbm_pause_eval();
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
				chThdSleepMilliseconds(100);
				timeout_cnt--;
			}
			ok = timeout_cnt > 0;
		} else {
			ok = start_lisp(true, true);
		}

		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = packet_id;
		send_buffer[ind++] = ok;
		reply_func(send_buffer, ind);
	} break;


	case COMM_LISP_GET_STATS: {

		float cpu_use = 0.0;
		float heap_use = 0.0;
		float mem_use = 0.0;
		float stack_use = 0.0;

		static systime_t time_last = 0;
		if (eval_tp) {
			cpu_use = 100.0 * (float)eval_tp->p_time / (float)(chVTGetSystemTimeX() - time_last);
			time_last = chVTGetSystemTimeX();
			eval_tp->p_time = 0;
		} else {
			break;
		}

		if (lbm_heap_state.gc_num > 0) {
			heap_use = 100.0 * (float)(HEAP_SIZE - lbm_heap_state.gc_last_free) / (float)HEAP_SIZE;
		}

		mem_use = 100.0 * (float)(lbm_memory_num_words() - lbm_memory_num_free()) / (float)lbm_memory_num_words();
		lbm_running_iterator(ctx_cb, &stack_use, NULL);

		chMtxLock(&send_buffer_mutex);
		int32_t ind = 0;

		send_buffer_global[ind++] = packet_id;
		buffer_append_float16(send_buffer_global, cpu_use, 1e2, &ind);
		buffer_append_float16(send_buffer_global, heap_use, 1e2, &ind);
		buffer_append_float16(send_buffer_global, mem_use, 1e2, &ind);
		buffer_append_float16(send_buffer_global, stack_use, 1e2, &ind);

		char r_buf[40];
		r_buf[0] = '\0'; lbm_done_iterator(ctx_cb, NULL, r_buf); r_buf[39] = '\0';
		strcpy((char*)(send_buffer_global + ind), r_buf); ind += strlen(r_buf) + 1;

		lbm_value curr = *lbm_get_env_ptr();
		while (lbm_type_of(curr) == LBM_TYPE_CONS) {
			lbm_value key_val = lbm_car(curr);
			if (lbm_type_of(lbm_car(key_val)) == LBM_TYPE_SYMBOL && lbm_is_number(lbm_cdr(key_val))) {
				const char *name = lbm_get_name_by_symbol(lbm_dec_sym(lbm_car(key_val)));
				strcpy((char*)(send_buffer_global + ind), name);
				ind += strlen(name) + 1;
				buffer_append_float32_auto(send_buffer_global, lbm_dec_as_float(lbm_cdr(key_val)), &ind);
			}

			if (ind > 300) {
				break;
			}

			curr = lbm_cdr(curr);
		}

		for (int i = 0; i < lbm_get_num_variables(); i ++) {
			const char *name = lbm_get_variable_name_by_index(i);
			const lbm_value var = lbm_get_variable_by_index(i);
			if (lbm_is_number(var) && name) {
				strcpy((char*)(send_buffer_global + ind), name);
				ind += strlen(name) + 1;
				buffer_append_float32_auto(send_buffer_global, lbm_dec_as_float(var), &ind);

				if (ind > 300) {
					break;
				}
			}
		}

		reply_func(send_buffer_global, ind);
		chMtxUnlock(&send_buffer_mutex);
	} break;

	case COMM_LISP_REPL_CMD: {
		if (!lisp_thd_running) {
			start_lisp(true, false);
		}

		if (lisp_thd_running) {
			char *str = (char*)data;

			if (len <= 1) {
				commands_printf_lisp(">");
			} else if (len >= 5 && strncmp(str, ":help", 5) == 0) {
				commands_printf_lisp("== Special Commands ==");
				commands_printf_lisp(
						":help\n"
						"  Print this help text");
				commands_printf_lisp(
						":info\n"
						"  Print info about memory usage, allocated arrays and garbage collection");
				commands_printf_lisp(
						":env\n"
						"  Print current environment and variables");
				commands_printf_lisp(
						":ctxs\n"
						"  Print context (threads) info");
				commands_printf_lisp(
						":symbols\n"
						"  Print symbol names");
				commands_printf_lisp(
						":reset\n"
						"  Reset LBM");
				commands_printf_lisp(
						":pause\n"
						"  Pause LBM");
				commands_printf_lisp(
						":continue\n"
						"  Continue running LBM");
				commands_printf_lisp(
						":step <num_steps>\n"
						"  Run num_steps LBM steps");
				commands_printf_lisp(
						":undef <symbol_name>\n"
						"  Undefine symbol");
				commands_printf_lisp(
						":verb\n"
						"  Toggle verbose error messages");
				commands_printf_lisp(" ");
				commands_printf_lisp("Anything else will be evaluated as an expression in LBM.");
				commands_printf_lisp(" ");
			} else if (len >= 5 && strncmp(str, ":info", 5) == 0) {
				commands_printf_lisp("--(LISP HEAP)-----------------------------------------------\n");
				commands_printf_lisp("Heap size: %u Bytes\n", HEAP_SIZE * 8);
				commands_printf_lisp("Used cons cells: %d\n", HEAP_SIZE - lbm_heap_num_free());
				commands_printf_lisp("Free cons cells: %d\n", lbm_heap_num_free());
				commands_printf_lisp("GC counter: %d\n", lbm_heap_state.gc_num);
				commands_printf_lisp("Recovered: %d\n", lbm_heap_state.gc_recovered);
				commands_printf_lisp("Recovered arrays: %u\n", lbm_heap_state.gc_recovered_arrays);
				commands_printf_lisp("Marked: %d\n", lbm_heap_state.gc_marked);
				commands_printf_lisp("--(Symbol and Array memory)---------------------------------\n");
				commands_printf_lisp("Memory size: %u Words\n", lbm_memory_num_words());
				commands_printf_lisp("Memory free: %u Words\n", lbm_memory_num_free());
				commands_printf_lisp("Allocated arrays: %u\n", lbm_heap_state.num_alloc_arrays);
				commands_printf_lisp("Symbol table size: %u Bytes\n", lbm_get_symbol_table_size());
			} else if (strncmp(str, ":env", 4) == 0) {
				lbm_value curr = *lbm_get_env_ptr();
				char output[128];

				commands_printf_lisp("Environment:\n");
				while (lbm_type_of(curr) == LBM_TYPE_CONS) {
					lbm_print_value(output, sizeof(output), lbm_car(curr));
					curr = lbm_cdr(curr);
					commands_printf_lisp("  %s",output);
				}
				commands_printf_lisp("Variables:");
				for (int i = 0; i < lbm_get_num_variables(); i ++) {
					const char *name = lbm_get_variable_name_by_index(i);
					lbm_print_value(output,1024, lbm_get_variable_by_index(i));
					commands_printf_lisp("  %s = %s", name ? name : "error", output);
				}
			} else if (strncmp(str, ":ctxs", 5) == 0) {
				commands_printf_lisp("****** Running contexts ******");
				lbm_running_iterator(print_ctx_info, NULL, NULL);
				commands_printf_lisp("****** Blocked contexts ******");
				lbm_blocked_iterator(print_ctx_info, NULL, NULL);
				commands_printf_lisp("****** Done contexts ******");
				lbm_done_iterator(print_ctx_info, NULL, NULL);
			} else if (strncmp(str, ":symbols", 8) == 0) {
				lbm_symrepr_name_iterator(sym_it);
				commands_printf_lisp(" ");
			} else if (strncmp(str, ":reset", 6) == 0) {
				commands_printf_lisp(start_lisp(false, flash_helper_code_size(CODE_IND_LISP) > 0) ?
						"Reset OK\n\n" : "Reset Failed\n\n");
			} else if (strncmp(str, ":pause", 6) == 0) {
				lbm_pause_eval_with_gc(30);
				while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
					sleep_callback(10);
				}
				commands_printf_lisp("Evaluator paused\n");
			} else if (strncmp(str, ":continue", 9) == 0) {
				lbm_continue_eval();
			} else if (strncmp(str, ":step", 5) == 0) {
				int num = atoi(str + 5);
				lbm_step_n_eval((uint32_t)num);
			} else if (strncmp(str, ":undef", 6) == 0) {
				lbm_pause_eval();
				while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
					sleep_callback(10);
				}
				char *sym = str + 7;
				commands_printf_lisp("undefining: %s", sym);
				commands_printf_lisp("%s", lbm_undefine(sym) ? "Cleared bindings" : "No definition found");
				lbm_continue_eval();
			} else if (strncmp(str, ":verb", 5) == 0) {
				static bool verbose_now = false;
				verbose_now = !verbose_now;
				lbm_set_verbose(verbose_now);
				commands_printf_lisp("Verbose errors %s", verbose_now ? "Enabled" : "Disabled");
			} else {
				bool ok = true;
				int timeout_cnt = 1000;
				lbm_pause_eval_with_gc(30);
				while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
					chThdSleep(5);
					timeout_cnt--;
				}
				ok = timeout_cnt > 0;

				if (ok) {
					lbm_create_char_stream_from_string(&string_tok_state, &string_tok, (char*)data);
					repl_cid = lbm_load_and_eval_expression(&string_tok);
					lbm_continue_eval();
					lbm_wait_ctx(repl_cid, 500);
					repl_cid = -1;
				} else {
					commands_printf_lisp("Could not pause");
				}
			}
		} else {
			commands_printf_lisp("LispBM is not running");
		}
	} break;

	default:
		break;
	}
}

static void done_callback(eval_context_t *ctx) {
	lbm_cid cid = ctx->id;
	lbm_value t = ctx->r;

	if (cid == repl_cid) {
		char output[128];
		lbm_print_value(output, sizeof(output), t);
		commands_printf_lisp("> %s", output);
	}
}

static bool start_lisp(bool print, bool load_code) {
	bool res = false;

	char *code_data = (char*)flash_helper_code_data(CODE_IND_LISP);
	int32_t code_len = flash_helper_code_size(CODE_IND_LISP);

	if (!load_code || (code_data != 0 && code_len > 0)) {
		if (!lisp_thd_running) {
			lbm_init(heap, HEAP_SIZE,
					gc_stack_storage, GC_STACK_SIZE,
					memory_array, LBM_MEMORY_SIZE_8K,
					bitmap_array, LBM_MEMORY_BITMAP_SIZE_8K,
					print_stack_storage, PRINT_STACK_SIZE,
					extension_storage, EXTENSION_STORAGE_SIZE);
			lbm_variables_init(variable_storage, VARIABLE_STORAGE_SIZE);

			lbm_set_timestamp_us_callback(timestamp_callback);
			lbm_set_usleep_callback(sleep_callback);
			lbm_set_printf_callback(commands_printf_lisp);
			lbm_set_ctx_done_callback(done_callback);
			chThdCreateStatic(eval_thread_wa, sizeof(eval_thread_wa), NORMALPRIO - 1, eval_thread, NULL);

			lisp_thd_running = true;
		} else {
			lbm_pause_eval();
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				chThdSleepMilliseconds(100);
			}

			lbm_init(heap, HEAP_SIZE,
					gc_stack_storage, GC_STACK_SIZE,
					memory_array, LBM_MEMORY_SIZE_8K,
					bitmap_array, LBM_MEMORY_BITMAP_SIZE_8K,
					print_stack_storage, PRINT_STACK_SIZE,
					extension_storage, EXTENSION_STORAGE_SIZE);
			lbm_variables_init(variable_storage, VARIABLE_STORAGE_SIZE);

			lbm_pause_eval();
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				chThdSleepMilliseconds(100);
			}
		}

		lispif_load_vesc_extensions();
		lbm_set_dynamic_load_callback(lispif_vesc_dynamic_loader);

		if (load_code) {
			if (print) {
				commands_printf_lisp("Parsing %d characters", strlen(code_data));
			}

			lbm_create_char_stream_from_string(&string_tok_state, &string_tok, code_data);
			lbm_load_and_eval_program(&string_tok);
		}

		lbm_continue_eval();

		res = true;
	}

	return res;
}

static uint32_t timestamp_callback(void) {
	systime_t t = chVTGetSystemTime();
	return (uint32_t) ((1000000 / CH_CFG_ST_FREQUENCY) * t);
}

static void sleep_callback(uint32_t us) {
	chThdSleepMicroseconds(us);
}

static THD_FUNCTION(eval_thread, arg) {
	(void)arg;
	eval_tp = chThdGetSelfX();
	chRegSetThreadName("Lisp Eval");
	lbm_run_eval();
}
