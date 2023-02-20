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
#include "mempools.h"

#define HEAP_SIZE				(2048 + 256 + 160)
#define LISP_MEM_SIZE			LBM_MEMORY_SIZE_16K
#define LISP_MEM_BITMAP_SIZE	LBM_MEMORY_BITMAP_SIZE_16K
#define GC_STACK_SIZE			160
#define PRINT_STACK_SIZE		128
#define EXTENSION_STORAGE_SIZE	250
#define VARIABLE_STORAGE_SIZE	64

__attribute__((section(".ram4"))) static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static uint32_t memory_array[LISP_MEM_SIZE];
static uint32_t bitmap_array[LISP_MEM_BITMAP_SIZE];
static uint32_t gc_stack_storage[GC_STACK_SIZE];
__attribute__((section(".ram4"))) static uint32_t print_stack_storage[PRINT_STACK_SIZE];
__attribute__((section(".ram4"))) static extension_fptr extension_storage[EXTENSION_STORAGE_SIZE];
__attribute__((section(".ram4"))) static lbm_value variable_storage[VARIABLE_STORAGE_SIZE];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;
static lbm_buffered_channel_state_t buffered_tok_state;
static lbm_char_channel_t buffered_string_tok;

static thread_t *eval_tp = 0;
static THD_FUNCTION(eval_thread, arg);
static THD_WORKING_AREA(eval_thread_wa, 2048);
static bool lisp_thd_running = false;
static mutex_t lbm_mutex;

static int repl_cid = -1;
static int restart_cnt = 0;

// Private functions
static uint32_t timestamp_callback(void);
static void sleep_callback(uint32_t us);

void lispif_init(void) {
	// Do not attempt to start lisp after a watchdog reset, in case lisp
	// was the cause of it.
	// TODO: Anything else to check?
	if (!timeout_had_IWDG_reset() && terminal_get_first_fault() != FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET) {
		lispif_restart(false, true);
	}

	lbm_set_eval_step_quota(50);

	chMtxObjectInit(&lbm_mutex);
}

int lispif_get_restart_cnt(void) {
	return restart_cnt;
}

void lispif_lock_lbm(void) {
	chMtxLock(&lbm_mutex);
}

void lispif_unlock_lbm(void) {
	chMtxUnlock(&lbm_mutex);
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
			int timeout_cnt = 2000;
			lbm_pause_eval();
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
				chThdSleepMilliseconds(1);
				timeout_cnt--;
			}
			ok = timeout_cnt > 0;
		} else {
			ok = lispif_restart(true, true);
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

		uint8_t *send_buffer_global = mempools_get_packet_buffer();
		int32_t ind = 0;

		send_buffer_global[ind++] = packet_id;
		buffer_append_float16(send_buffer_global, cpu_use, 1e2, &ind);
		buffer_append_float16(send_buffer_global, heap_use, 1e2, &ind);
		buffer_append_float16(send_buffer_global, mem_use, 1e2, &ind);

		// Stack. Currently unused
		buffer_append_float16(send_buffer_global, 0, 1e2, &ind);

		// Result. Currently unused
		send_buffer_global[ind++] = '\0';

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
		mempools_free_packet_buffer(send_buffer_global);
	} break;

	case COMM_LISP_REPL_CMD: {
		if (!lisp_thd_running) {
			lispif_restart(true, false);
		}

		if (lisp_thd_running) {
			lispif_lock_lbm();
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
						":undef <symbol_name>\n"
						"  Undefine symbol");
				commands_printf_lisp(
						":verb\n"
						"  Toggle verbose error messages");
				commands_printf_lisp(" ");
				commands_printf_lisp("Anything else will be evaluated as an expression in LBM.");
				commands_printf_lisp(" ");
			} else if (len >= 5 && strncmp(str, ":info", 5) == 0) {
				commands_printf_lisp("--(LISP HEAP)--\n");
				commands_printf_lisp("Heap size: %u Bytes\n", HEAP_SIZE * 8);
				commands_printf_lisp("Used cons cells: %d\n", HEAP_SIZE - lbm_heap_num_free());
				commands_printf_lisp("Free cons cells: %d\n", lbm_heap_num_free());
				commands_printf_lisp("GC counter: %d\n", lbm_heap_state.gc_num);
				commands_printf_lisp("Recovered: %d\n", lbm_heap_state.gc_recovered);
				commands_printf_lisp("Recovered arrays: %u\n", lbm_heap_state.gc_recovered_arrays);
				commands_printf_lisp("Marked: %d\n", lbm_heap_state.gc_marked);
				commands_printf_lisp("--(Symbol and Array memory)--\n");
				commands_printf_lisp("Memory size: %u Words\n", lbm_memory_num_words());
				commands_printf_lisp("Memory free: %u Words\n", lbm_memory_num_free());
				commands_printf_lisp("Allocated arrays: %u\n", lbm_heap_state.num_alloc_arrays);
				commands_printf_lisp("Symbol table size: %u Bytes\n", lbm_get_symbol_table_size());
				commands_printf_lisp("Extensions: %u, max %u\n", lbm_get_num_extensions(), lbm_get_max_extensions());
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
				commands_printf_lisp("****** Sleeping contexts ******");
				lbm_sleeping_iterator(print_ctx_info, NULL, NULL);
			} else if (strncmp(str, ":symbols", 8) == 0) {
				lbm_symrepr_name_iterator(sym_it);
				commands_printf_lisp(" ");
			} else if (strncmp(str, ":reset", 6) == 0) {
				commands_printf_lisp(lispif_restart(false, flash_helper_code_size(CODE_IND_LISP) > 0) ?
						"Reset OK\n\n" : "Reset Failed\n\n");
			} else if (strncmp(str, ":pause", 6) == 0) {
				lbm_pause_eval_with_gc(30);
				while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
					lbm_pause_eval();
					sleep_callback(1);
				}
				commands_printf_lisp("Evaluator paused\n");
			} else if (strncmp(str, ":continue", 9) == 0) {
				lbm_continue_eval();
			} else if (strncmp(str, ":undef", 6) == 0) {
				lbm_pause_eval();
				while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
					lbm_pause_eval();
					sleep_callback(1);
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
					lbm_create_string_char_channel(&string_tok_state, &string_tok, (char*)data);
					if (reply_func != NULL) {
						repl_cid = lbm_load_and_eval_expression(&string_tok);
						lbm_continue_eval();
						lbm_wait_ctx(repl_cid, 500);
					}
					repl_cid = -1;
				} else {
					commands_printf_lisp("Could not pause");
				}
			}
			lispif_unlock_lbm();
		} else {
			commands_printf_lisp("LispBM is not running");
		}
	} break;

	case COMM_LISP_STREAM_CODE: {
		int32_t ind = 0;
		int32_t offset = buffer_get_int32(data, &ind);
		int32_t tot_len = buffer_get_int32(data, &ind);
		int8_t restart = data[ind++];

		static bool buffered_channel_created = false;
		static int32_t offset_last = -1;
		static int16_t result_last = -1;

		if (offset == 0) {
			if (!lisp_thd_running) {
				lispif_restart(true, restart == 2 ? true : false);
				buffered_channel_created = false;
			} else if (restart == 1) {
				lispif_restart(true, false);
				buffered_channel_created = false;
			} else if (restart == 2) {
				lispif_restart(true, true);
				buffered_channel_created = false;
			}
		}

		int32_t send_ind = 0;
		uint8_t send_buffer[50];
		send_buffer[send_ind++] = packet_id;
		buffer_append_int32(send_buffer, offset, &send_ind);

		if (offset_last == offset) {
			buffer_append_int16(send_buffer, result_last, &send_ind);
			reply_func(send_buffer, ind);
			break;
		}

		offset_last = offset;

		if (!lisp_thd_running) {
			result_last = -1;
			offset_last = -1;
			buffer_append_int16(send_buffer, result_last, &send_ind);
			reply_func(send_buffer, ind);
			break;
		}

		if (offset == 0) {
			if (buffered_channel_created) {
				int timeout = 1500;
				while (!buffered_tok_state.reader_closed) {
					lbm_channel_writer_close(&buffered_string_tok);
					chThdSleepMilliseconds(1);
					timeout--;
					if (timeout == 0) {
						break;
					}
				}

				if (timeout == 0) {
					result_last = -2;
					offset_last = -1;
					buffer_append_int16(send_buffer, result_last, &send_ind);
					commands_printf_lisp("Reader not closing");
					reply_func(send_buffer, ind);
					break;
				}
			}

			int timeout_cnt = 1000;
			lispif_lock_lbm();
			lbm_pause_eval_with_gc(30);
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
				chThdSleep(5);
				timeout_cnt--;
			}

			if (timeout_cnt == 0) {
				lispif_unlock_lbm();
				result_last = -3;
				offset_last = -1;
				buffer_append_int16(send_buffer, result_last, &send_ind);
				commands_printf_lisp("Could not pause");
				reply_func(send_buffer, ind);
				break;
			}

			lbm_create_buffered_char_channel(&buffered_tok_state, &buffered_string_tok);

			if (lbm_load_and_eval_program(&buffered_string_tok) <= 0) {
				lispif_unlock_lbm();
				result_last = -4;
				offset_last = -1;
				buffer_append_int16(send_buffer, result_last, &send_ind);
				commands_printf_lisp("Could not start eval");
				reply_func(send_buffer, ind);
				break;
			}

			lbm_continue_eval();
			buffered_channel_created = true;
			lispif_unlock_lbm();
		}

		int32_t written = 0;
		int timeout = 1500;
		while (ind < (int32_t)len) {
			int ch_res = lbm_channel_write(&buffered_string_tok, (char)data[ind]);

			if (ch_res == CHANNEL_SUCCESS) {
				ind++;
				written++;
				timeout = 0;
			} else if (ch_res == CHANNEL_READER_CLOSED) {
				break;
			} else {
				chThdSleepMilliseconds(1);
				timeout--;
				if (timeout == 0) {
					break;
				}
			}
		}

		if (ind == (int32_t)len) {
			if ((offset + written) == tot_len) {
				lbm_channel_writer_close(&buffered_string_tok);
				offset_last = -1;
				commands_printf_lisp("Stream done, starting...");
			}

			result_last = 0;
			buffer_append_int16(send_buffer, result_last, &send_ind);
		} else {
			if (timeout == 0) {
				result_last = -5;
				offset_last = -1;
				buffer_append_int16(send_buffer, result_last, &send_ind);
				commands_printf_lisp("Stream timed out");
			} else {
				result_last = -6;
				offset_last = -1;
				buffer_append_int16(send_buffer, result_last, &send_ind);
				commands_printf_lisp("Stream closed");
			}
		}

		reply_func(send_buffer, send_ind);
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

bool lispif_restart(bool print, bool load_code) {
	bool res = false;

	restart_cnt++;

	lispif_stop_lib();

	char *code_data = (char*)flash_helper_code_data(CODE_IND_LISP);
	int32_t code_len = flash_helper_code_size(CODE_IND_LISP);

	if (!load_code || (code_data != 0 && code_len > 0)) {
		lispif_disable_all_events();

		if (!lisp_thd_running) {
			lbm_init(heap, HEAP_SIZE,
					gc_stack_storage, GC_STACK_SIZE,
					memory_array, LISP_MEM_SIZE,
					bitmap_array, LISP_MEM_BITMAP_SIZE,
					print_stack_storage, PRINT_STACK_SIZE,
					extension_storage, EXTENSION_STORAGE_SIZE);
			lbm_variables_init(variable_storage, VARIABLE_STORAGE_SIZE);
			lbm_eval_init_events(20);

			lbm_set_timestamp_us_callback(timestamp_callback);
			lbm_set_usleep_callback(sleep_callback);
			lbm_set_printf_callback(commands_printf_lisp);
			lbm_set_ctx_done_callback(done_callback);
			chThdCreateStatic(eval_thread_wa, sizeof(eval_thread_wa), NORMALPRIO - 1, eval_thread, NULL);

			lisp_thd_running = true;
		} else {
			lbm_pause_eval();
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				lbm_pause_eval();
				chThdSleepMilliseconds(1);
			}

			lbm_init(heap, HEAP_SIZE,
					gc_stack_storage, GC_STACK_SIZE,
					memory_array, LISP_MEM_SIZE,
					bitmap_array, LISP_MEM_BITMAP_SIZE,
					print_stack_storage, PRINT_STACK_SIZE,
					extension_storage, EXTENSION_STORAGE_SIZE);
			lbm_variables_init(variable_storage, VARIABLE_STORAGE_SIZE);
			lbm_eval_init_events(20);
		}

		lbm_pause_eval();
		while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
			lbm_pause_eval();
			chThdSleepMilliseconds(1);
		}

		lispif_load_vesc_extensions();
		lbm_set_dynamic_load_callback(lispif_vesc_dynamic_loader);

		int code_chars = strnlen(code_data, code_len);

		// Load imports
		if (code_len > code_chars + 3) {
			int32_t ind = code_chars + 1;
			uint16_t num_imports = buffer_get_uint16((uint8_t*)code_data, &ind);

			if (num_imports > 0 && num_imports < 500) {
				for (int i = 0;i < num_imports;i++) {
					char *name = code_data + ind;
					ind += strlen(name) + 1;
					int32_t offset = buffer_get_int32((uint8_t*)code_data, &ind);
					int32_t len = buffer_get_int32((uint8_t*)code_data, &ind);

					lbm_value val;
					if (lbm_share_array(&val, code_data + offset, LBM_TYPE_BYTE, len)) {
						lbm_define(name, val);
					}
				}
			}
		}

		if (load_code) {
			if (print) {
				commands_printf_lisp("Parsing %d characters", code_chars);
			}

			lbm_create_string_char_channel(&string_tok_state, &string_tok, code_data);
			lbm_load_and_eval_program(&string_tok);
		}

		lbm_continue_eval();

		res = true;
	}

	return res;
}

static uint32_t timestamp_callback(void) {
	systime_t t = chVTGetSystemTimeX();
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
