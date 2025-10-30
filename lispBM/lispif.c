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

#pragma GCC optimize ("Os")

#include "lispif.h"
#include "lbm_image.h"
#include "commands.h"
#include "terminal.h"
#include "flash_helper.h"
#include "buffer.h"
#include "timeout.h"
#include "lispbm.h"
#include "mempools.h"
#include "stm32f4xx_conf.h"
#include "lbm_prof.h"
#include "utils.h"

#define LBM_MEMORY_SIZE_28K LBM_MEMORY_SIZE_64BYTES_TIMES_X(448)
#define LBM_MEMORY_BITMAP_SIZE_28K LBM_MEMORY_BITMAP_SIZE(448)

#ifndef EXTENSION_STORAGE_SIZE
#define EXTENSION_STORAGE_SIZE		311
#endif

#ifndef ADC_SAMPLE_MAX_LEN
#define ADC_SAMPLE_MAX_LEN			1000 // 20 byte per sample
#endif

#define HEAP_SIZE					(((1024 * 24 + (1000 - ADC_SAMPLE_MAX_LEN) * 20) - (EXTENSION_STORAGE_SIZE * sizeof(lbm_extension_t))) / sizeof(lbm_cons_t))
#define LISP_MEM_SIZE				LBM_MEMORY_SIZE_28K
#define LISP_MEM_BITMAP_SIZE		LBM_MEMORY_BITMAP_SIZE_28K
#define GC_STACK_SIZE				160
#define PRINT_STACK_SIZE			128
#define EXT_LOAD_CALLBACK_LEN		20
#define PROF_DATA_NUM				30

__attribute__((section(".ram4"))) static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static uint32_t memory_array[LISP_MEM_SIZE];
__attribute__((section(".ram4"))) static uint32_t bitmap_array[LISP_MEM_BITMAP_SIZE];
__attribute__((section(".ram4"))) static lbm_extension_t extension_storage[EXTENSION_STORAGE_SIZE];
__attribute__((section(".ram4"))) static lbm_prof_t prof_data[PROF_DATA_NUM];
static volatile bool prof_running = false;

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;
static lbm_buffered_channel_state_t buffered_tok_state;
static lbm_char_channel_t buffered_string_tok;
static bool string_tok_valid = false;

static volatile lbm_uint *image_ptr = 0;
static int image_max_ind = 0;

static thread_t *eval_tp = 0;
static THD_FUNCTION(eval_thread, arg);
__attribute__((section(".ram4"))) static THD_WORKING_AREA(eval_thread_wa, 2048);
static volatile bool lisp_thd_running = false;
static mutex_t lbm_mutex;

static lbm_cid repl_cid = -1;
static lbm_cid main_cid = -1;
static lbm_cid repl_cid_for_buffer = -1;
static char *repl_buffer = 0;
static volatile systime_t repl_time = 0;
static int restart_cnt = 0;
static volatile bool const_write_error = false;

// Private functions
static void sleep_callback(uint32_t us);
static bool image_write(uint32_t w, int32_t ix, bool const_heap);

// Extension load callbacks
void(*ext_load_callbacks[EXT_LOAD_CALLBACK_LEN])(bool) = {0};

// Global
extern lbm_const_heap_t *lbm_const_heap_state;

void lispif_init(void) {
	// Do not attempt to start lisp after a watchdog reset, in case lisp
	// was the cause of it.
	// TODO: Anything else to check?
	if (!timeout_had_IWDG_reset() && terminal_get_first_fault() != FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET) {
		lispif_restart(false, true, true);
	}

#ifdef LBM_USE_TIME_QUOTA
	lbm_set_eval_time_quota(2000);
#else
	lbm_set_eval_step_quota(50);
#endif

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

	if (ctx->name) {
		commands_printf_lisp("Context Name: %s", ctx->name);
	}

	commands_printf_lisp("Stack SP: %u",  ctx->K.sp);
	commands_printf_lisp("Stack SP max: %u", lbm_get_max_stack(&ctx->K));
	commands_printf_lisp("Result%s: %s", print_ret ? "" : " (trunc)", output);
}

static void sym_it(const char *str) {
	bool sym_name_flash = lbm_symbol_in_flash((char *)str);
	bool sym_entry_flash = lbm_symbol_list_entry_in_flash((char *)str);
	commands_printf_lisp("[Name: %s, Entry: %s]: %s\n",
			sym_name_flash ? "FLASH" : "L_MEM",
					sym_entry_flash ? "FLASH" : "L_MEM",
							str);
}

static void prof_thd_wrapper(void *v) {
	(void)v;

	while (prof_running) {
		lbm_prof_sample();
		chThdSleepMicroseconds(200);
	}
}

static bool pause_eval(uint32_t num_free, uint32_t timeout_ms) {
	if (!lisp_thd_running) {
		return false;
	}

	int timeout_cnt = timeout_ms;

	if (num_free > 0) {
		lbm_pause_eval_with_gc(num_free);
	} else {
		lbm_pause_eval();
	}

	while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
		chThdSleepMilliseconds(1);
		timeout_cnt--;
	}

	return timeout_cnt > 0;
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

		if (!running) {
			ok = pause_eval(0, 2000);
		} else {
			ok = lispif_restart(true, true, true);
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
		utils_sys_lock_cnt();
		if (eval_tp) {
			cpu_use = 100.0 * (float)eval_tp->p_time / (float)(chVTGetSystemTimeX() - time_last);
			time_last = chVTGetSystemTimeX();
			eval_tp->p_time = 0;
		} else {
			utils_sys_unlock_cnt();
			break;
		}
		utils_sys_unlock_cnt();

		bool print_all = true;
		if (len > 0) {
			print_all = data[0];
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

		if (pause_eval(0, 2000)) {
			lbm_value *glob_env = lbm_get_global_env();
			for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
				if (ind > 300) {
					break;
				}

				lbm_value curr = glob_env[i];
				while (lbm_type_of(curr) == LBM_TYPE_CONS) {
					lbm_value key_val = lbm_car(curr);
					if (lbm_type_of(lbm_car(key_val)) == LBM_TYPE_SYMBOL && lbm_is_number(lbm_cdr(key_val))) {
						const char *name = lbm_get_name_by_symbol(lbm_dec_sym(lbm_car(key_val)));

						if (print_all ||
								((name[0] == 'v' || name[0] == 'V') &&
										(name[1] == 't' || name[1] == 'T'))) {
							strcpy((char*)(send_buffer_global + ind), name);
							ind += strlen(name) + 1;
							buffer_append_float32_auto(send_buffer_global, lbm_dec_as_float(lbm_cdr(key_val)), &ind);
						}
					}

					if (ind > 300) {
						break;
					}

					curr = lbm_cdr(curr);
				}
			}
		}

		lbm_continue_eval();

		reply_func(send_buffer_global, ind);
		mempools_free_packet_buffer(send_buffer_global);
	} break;

	case COMM_LISP_REPL_CMD: {
		if (UTILS_AGE_S(repl_time) <= 0.5) {
			return;
		}

		if (!lisp_thd_running) {
			lispif_restart(true, false, true);
		}

		if (lisp_thd_running) {
			lispif_lock_lbm();
			char *str = (char*)data;

			if (len <= 1) {
				commands_printf_lisp(">");
			} else if (strncmp(str, ":help", 5) == 0) {
				commands_printf_lisp("== Special Commands ==");
				commands_printf_lisp(
						":help\n"
						"  Print this help text");
				commands_printf_lisp(
						":info\n"
						"  Print info about memory usage, allocated arrays and garbage collection");
				commands_printf_lisp(
						":prof start\n"
						"  Start profiler");
				commands_printf_lisp(
						":prof stop\n"
						"  Stop profiler");
				commands_printf_lisp(
						":prof report\n"
						"  Print profiler report");
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
			} else if (strncmp(str, ":info", 5) == 0) {
				commands_printf_lisp("--(LISP HEAP)--\n");
				commands_printf_lisp("Heap size: %u Bytes\n", HEAP_SIZE * 8);
				commands_printf_lisp("Used cons cells: %d\n", HEAP_SIZE - lbm_heap_num_free());
				commands_printf_lisp("Free cons cells: %d\n", lbm_heap_num_free());
				commands_printf_lisp("GC counter: %d\n", lbm_heap_state.gc_num);
				commands_printf_lisp("Recovered: %d\n", lbm_heap_state.gc_recovered);
				commands_printf_lisp("Recovered arrays: %u\n", lbm_heap_state.gc_recovered_arrays);
				commands_printf_lisp("Marked: %d\n", lbm_heap_state.gc_marked);
				commands_printf_lisp("GC SP max: %u (size %u)\n", lbm_get_max_stack(&lbm_heap_state.gc_stack), lbm_heap_state.gc_stack.size);
				commands_printf_lisp("Global env cells: %u\n", lbm_get_global_env_size());
				commands_printf_lisp("--(Symbol and Array memory)--\n");
				commands_printf_lisp("Memory size: %u bytes\n", lbm_memory_num_words() * 4);
				commands_printf_lisp("Memory free: %u bytes\n", lbm_memory_num_free() * 4);
				commands_printf_lisp("Longest block free: %u bytes\n", lbm_memory_longest_free() * 4);
				commands_printf_lisp("Allocated arrays: %u\n", lbm_heap_state.num_alloc_arrays);
				commands_printf_lisp("Symbol table size: %u Bytes\n", lbm_get_symbol_table_size());
				commands_printf_lisp("Symbol table size flash: %u Bytes\n", lbm_get_symbol_table_size_flash());
				commands_printf_lisp("Symbol name size: %u Bytes\n", lbm_get_symbol_table_size_names());
				commands_printf_lisp("Symbol name size flash: %u Bytes\n", lbm_get_symbol_table_size_names_flash());
				commands_printf_lisp("Extensions: %u, max %u\n", lbm_get_num_extensions(), lbm_get_max_extensions());
				commands_printf_lisp("--(Flash)--\n");
				int32_t image_size = lbm_image_get_size() - lbm_image_get_write_index();
				commands_printf_lisp("Code+Import: %d\n", flash_helper_code_size(CODE_IND_LISP));
				commands_printf_lisp("Const Heap : %d\n", lbm_const_heap_state->next * 4);
				commands_printf_lisp("Image      : %d\n", image_size * 4);
				commands_printf_lisp("Free       : %d\n", (lbm_image_get_size() - lbm_const_heap_state->next - image_size) * 4);
				commands_printf_lisp("ImageVer   : %s\n", lbm_image_get_version());
			} else if (strncmp(str, ":prof start", 11) == 0) {
				if (prof_running) {
					lbm_prof_init(prof_data, PROF_DATA_NUM);
					commands_printf_lisp("Profiler restarted\n");
				} else {
					lbm_prof_init(prof_data, PROF_DATA_NUM);
					prof_running = true;
					if (lispif_spawn(prof_thd_wrapper, 1024, "LBM Profiler", NULL)) {
						commands_printf_lisp("Profiler started\n");
					} else {
						commands_printf_lisp("Could not start profiler, most likely out of memory\n");
					}
				}
			} else if (strncmp(str, ":prof stop", 10) == 0) {
				commands_printf_lisp("Profiler stopped. Issue command ':prof report' for statistics\n");
				prof_running = false;
			} else if (strncmp(str, ":prof report", 12) == 0) {
				lbm_uint num_sleep = lbm_prof_get_num_sleep_samples();
				lbm_uint num_system = lbm_prof_get_num_system_samples();
				lbm_uint tot_samples = lbm_prof_get_num_samples();
				lbm_uint tot_gc = 0;
				commands_printf_lisp("CID\tName\tSamples\t%%Load\t%%GC");
				for (int i = 0; i < PROF_DATA_NUM; i ++) {
					if (prof_data[i].cid == -1) break;
					tot_gc += prof_data[i].gc_count;
					commands_printf_lisp("%d\t%s\t%u\t%.3f\t%.3f",
							prof_data[i].cid,
							prof_data[i].name,
							prof_data[i].count,
							(double)(100.0 * ((float)prof_data[i].count) / (float) tot_samples),
							(double)(100.0 * ((float)prof_data[i].gc_count) / (float)prof_data[i].count));
				}
				commands_printf_lisp(" ");
				commands_printf_lisp("GC:\t%u\t%f%%\n", tot_gc, (double)(100.0 * ((float)tot_gc / (float)tot_samples)));
				commands_printf_lisp("System:\t%u\t%f%%\n", num_system, (double)(100.0 * ((float)num_system / (float)tot_samples)));
				commands_printf_lisp("Sleep:\t%u\t%f%%\n", num_sleep, (double)(100.0 * ((float)num_sleep / (float)tot_samples)));
				commands_printf_lisp("Total:\t%u samples\n", tot_samples);
			} else if (strncmp(str, ":env", 4) == 0) {
				if (pause_eval(0, 1000)) {
					lbm_value *glob_env = lbm_get_global_env();
					char output[128];
					for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
						lbm_value curr = glob_env[i];
						while (lbm_type_of(curr) == LBM_TYPE_CONS) {
							lbm_print_value(output, sizeof(output), lbm_car(curr));
							curr = lbm_cdr(curr);

							commands_printf_lisp("  %s", output);
						}
					}
				}
			} else if (strncmp(str, ":ctxs", 5) == 0) {
				commands_printf_lisp("****** Running contexts ******");
				lbm_running_iterator(print_ctx_info, NULL, NULL);
				commands_printf_lisp("****** Blocked contexts ******");
				lbm_blocked_iterator(print_ctx_info, NULL, NULL);
			} else if (strncmp(str, ":symbols", 8) == 0) {
				if (pause_eval(0, 1000)) {
					lbm_symrepr_name_iterator(sym_it);
					commands_printf_lisp(" ");
				}
			} else if (strncmp(str, ":reset", 6) == 0) {
				lispif_unlock_lbm();
				commands_printf_lisp(lispif_restart(true, flash_helper_code_size(CODE_IND_LISP) > 0, true) ?
						"Reset OK\n\n" : "Reset Failed\n\n");
				lispif_lock_lbm();
			} else if (strncmp(str, ":pause", 6) == 0) {
				if (pause_eval(30, 1000)) {
					commands_printf_lisp("Evaluator paused\n");
				}
			} else if (strncmp(str, ":continue", 9) == 0) {
				lbm_continue_eval();
			} else if (strncmp(str, ":undef", 6) == 0) {
				if (pause_eval(30, 1000)) {
					char *sym = str + 7;
					commands_printf_lisp("undefining: %s", sym);
					commands_printf_lisp("%s", lbm_undefine(sym) ? "Cleared bindings" : "No definition found");
					lbm_continue_eval();
				}
			} else if (strncmp(str, ":verb", 5) == 0) {
				static bool verbose_now = false;
				verbose_now = !verbose_now;
				lbm_set_verbose(verbose_now);
				commands_printf_lisp("Verbose errors %s", verbose_now ? "Enabled" : "Disabled");
			} else {
				if (repl_buffer) {
					lispif_unlock_lbm();
					break;
				}

				if (pause_eval(30, 1000)) {
					repl_buffer = lbm_malloc_reserve(len);
					if (repl_buffer) {
						memcpy(repl_buffer, data, len);
						lbm_create_string_char_channel(&string_tok_state, &string_tok, repl_buffer);
						repl_cid = lbm_load_and_eval_expression(&string_tok);
						repl_cid_for_buffer = repl_cid;
						lbm_image_save_constant_heap_ix();
						lbm_continue_eval();

						if (reply_func != NULL) {
							repl_time = chVTGetSystemTimeX();
						} else {
							repl_cid = -1;
						}
					} else {
						commands_printf_lisp("Not enough memory");
					}
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

		static int32_t offset_last = -1;
		static int16_t result_last = -1;

		if (offset == 0) {
			if (!lisp_thd_running) {
				lispif_restart(true, restart == 2 ? true : false, true);
			} else if (restart == 1) {
				lispif_restart(true, false, true);
			} else if (restart == 2) {
				lispif_restart(true, true, true);
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
			if (string_tok_valid) {
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

			lispif_lock_lbm();

			if (!pause_eval(30, 1000)) {
				lispif_unlock_lbm();
				result_last = -3;
				offset_last = -1;
				buffer_append_int16(send_buffer, result_last, &send_ind);
				commands_printf_lisp("Could not pause");
				reply_func(send_buffer, ind);
				break;
			}

			lbm_create_buffered_char_channel(&buffered_tok_state, &buffered_string_tok);
			string_tok_valid = true;

			if (lbm_load_and_eval_program(&buffered_string_tok, "main-s") <= 0) {
				lispif_unlock_lbm();
				result_last = -4;
				offset_last = -1;
				buffer_append_int16(send_buffer, result_last, &send_ind);
				commands_printf_lisp("Could not start eval");
				reply_func(send_buffer, ind);
				break;
			}

			lbm_continue_eval();
			lispif_unlock_lbm();
		}

		if (!string_tok_valid) {
			result_last = -15;
			buffer_append_int16(send_buffer, result_last, &send_ind);
			commands_printf_lisp("Tokenizer Invalid");
			reply_func(send_buffer, ind);
			break;
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
				lbm_image_save_constant_heap_ix();
				string_tok_valid = false;
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

	case COMM_LISP_RMSG: {
		lispif_process_rmsg(data[0], data + 1, len - 1);
	} break;

	default:
		break;
	}
}

static void done_callback(eval_context_t *ctx) {
	lbm_cid cid = ctx->id;
	lbm_value t = ctx->r;

	if (cid == repl_cid) {
		if (UTILS_AGE_S(repl_time) < 0.5) {
			char output[128];
			lbm_print_value(output, sizeof(output), t);
			commands_printf_lisp("> %s", output);
		} else {
			repl_cid = -1;
		}
	}

	if (cid == repl_cid_for_buffer && repl_buffer) {
		lbm_free(repl_buffer);
		repl_buffer = 0;
	}

	if (cid == main_cid) {
		lbm_image_save_constant_heap_ix();
		main_cid = -1;
	}
}

void lispif_stop(void) {
	if (!lisp_thd_running) {
		return;
	}

	lispif_stop_lib();

	lispif_lock_lbm();

	lbm_kill_eval();
	while (lisp_thd_running) {
		lbm_kill_eval();
		chThdSleepMilliseconds(1);
	}

	lispif_unlock_lbm();
}

bool lispif_restart(bool print, bool load_code, bool load_imports) {
	bool res = false;

	restart_cnt++;
	prof_running = false;
	string_tok_valid = false;

	char *code_data = (char*)flash_helper_code_data(CODE_IND_LISP);
	int32_t code_len = flash_helper_code_size(CODE_IND_LISP);

	if (!load_code || (code_data != 0 && code_len > 0)) {
		lispif_disable_all_events();

		if (const_write_error) {
			const_write_error = false;
			flash_helper_erase_code(CODE_IND_LISP_CONST);
		}

		bool save_heap = lisp_thd_running && lbm_image_exists();

		lispif_stop();

		if (save_heap) {
			lbm_image_save_constant_heap_ix();
		}

		int code_chars = 0;
		if (code_data) {
			code_chars = strnlen(code_data, code_len);
		} else {
			code_data = (char*)flash_helper_code_data_raw(CODE_IND_LISP);
		}

		bool new_image_created = false;
		image_max_ind = 0;
		image_ptr = (lbm_uint*)flash_helper_code_data_raw(CODE_IND_LISP_CONST);

		uint32_t image_len = 128 * 256; // 128k, size in words

		lbm_init(heap, HEAP_SIZE,
				memory_array, LISP_MEM_SIZE,
				bitmap_array, LISP_MEM_BITMAP_SIZE,
				GC_STACK_SIZE,
				PRINT_STACK_SIZE, extension_storage,
				EXTENSION_STORAGE_SIZE);

		lbm_set_usleep_callback(sleep_callback);
		lbm_set_printf_callback(commands_printf_lisp);
		lbm_set_ctx_done_callback(done_callback);

		lbm_image_init((uint32_t*)image_ptr, image_len, image_write);

		char ver_str[20];
		sprintf(ver_str, "%08X", (unsigned int)flash_helper_app_crc());

		bool load_imports_before = load_imports;
		load_imports = false;

		if (!lbm_image_exists() || strcmp(lbm_image_get_version(), ver_str) != 0) {
			commands_printf_lisp("Preparing new image...");
			flash_helper_erase_code(CODE_IND_LISP_CONST);
			image_max_ind = 0;
			lbm_image_create(ver_str);
			load_imports = load_imports_before;
			new_image_created = true;
		}

		lbm_image_boot();
		lbm_add_eval_symbols();
		lbm_eval_init_events(30);

		chThdCreateStatic(eval_thread_wa, sizeof(eval_thread_wa), NORMALPRIO - 1, eval_thread, NULL);
		lisp_thd_running = true;

		lbm_pause_eval();
		while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
			chThdSleepMilliseconds(1);
		}

		bool main_found = false;
		lbm_uint main_sym = ENC_SYM_NIL;
		if (lbm_get_symbol_by_name("main", &main_sym)) {
			lbm_value binding;
			if (lbm_global_env_lookup(&binding, lbm_enc_sym(main_sym))) {
				if (lbm_is_cons(binding) && lbm_car(binding) == ENC_SYM_CLOSURE) {
					code_data = "(main)";
					main_found = true;
				}
			}
		}

		lispif_load_vesc_extensions(main_found);

		for (int i = 0;i < EXT_LOAD_CALLBACK_LEN;i++) {
			if (ext_load_callbacks[i] == 0) {
				break;
			}

			ext_load_callbacks[i](main_found);
		}

		if (load_imports) {
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
						if (lbm_share_array_const(&val, code_data + offset, len)) {
							lbm_define(name, val);
						}
					}
				}

				lbm_image_save_global_env();
			}
		}

		if (load_code) {
			if (print) {
				if (main_found) {
					commands_printf_lisp("Running main-function");
				} else {
					commands_printf_lisp("Parsing %d characters", code_chars);
				}
			}

			lbm_create_string_char_channel(&string_tok_state, &string_tok, code_data);
			main_cid = lbm_load_and_eval_program_incremental(&string_tok, "main-u");

			// The first time a new image is created we save the const heap ptr after main exits. This makes
			// it more likely to work with code using const blocks from before there were images. We do
			// not want to store the const heap pointer each reboot as that will consume more flash with
			// each boot.
			if (!new_image_created) {
				main_cid = -1;
			}
		}

		lbm_continue_eval();

		res = true;
	}

	if (repl_buffer) {
		lbm_free(repl_buffer);
		repl_buffer = 0;
	}

	return res;
}

void lispif_add_ext_load_callback(void (*p_func)(bool)) {
	for (int i = 0;i < EXT_LOAD_CALLBACK_LEN;i++) {
		if (ext_load_callbacks[i] == 0 || ext_load_callbacks[i] == p_func) {
			ext_load_callbacks[i] = p_func;
			break;
		}
	}
}

bool lispif_is_eval_task(void) {
	return eval_tp == chThdGetSelfX();
}

lbm_uint lispif_const_heap_max_ind(void)  {
	return image_max_ind;
}

static void sleep_callback(uint32_t us) {
	chThdSleepMicroseconds(us);
}

static bool image_write(uint32_t w, int32_t ix, bool const_heap) {
	if (const_heap && ix > image_max_ind) {
		image_max_ind = ix;
	}

	if (image_ptr[ix] == w) {
		return true;
	}

	if (image_ptr[ix] != 0xffffffff) {
		commands_printf_lisp("Attempted to write to const heap at %d, but it is already occupied", ix);
		const_write_error = true;
		return false;
	}

	FLASH_Unlock();
	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);
	FLASH_ProgramWord((uint32_t)(image_ptr + ix), w);
	FLASH_Lock();

	if (image_ptr[ix] != w) {
		return false;
	}

	return true;
}

static THD_FUNCTION(eval_thread, arg) {
	(void)arg;
	eval_tp = chThdGetSelfX();
	chRegSetThreadName("Lisp Eval");
	lbm_run_eval();
	lisp_thd_running = false;
	eval_tp = 0;
}
