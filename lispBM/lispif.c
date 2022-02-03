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

#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "lbm_memory.h"
#include "env.h"
#include "lispbm.h"

#define HEAP_SIZE				1536
#define LISP_MEM_SIZE			LBM_MEMORY_SIZE_8K
#define LISP_MEM_BITMAP_SIZE	LBM_MEMORY_BITMAP_SIZE_8K

__attribute__((section(".ram4"))) static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static uint32_t memory_array[LISP_MEM_SIZE];
static uint32_t bitmap_array[LISP_MEM_BITMAP_SIZE];
static lbm_tokenizer_string_state_t string_tok_state;
static lbm_tokenizer_char_stream_t string_tok;

static thread_t *eval_tp = 0;
static THD_FUNCTION(eval_thread, arg);
static THD_WORKING_AREA(eval_thread_wa, 2048);
static bool lisp_thd_running = false;

// Private functions
static bool start_lisp(bool print);
static uint32_t timestamp_callback(void);
static void sleep_callback(uint32_t us);

void lispif_init(void) {
	// Do not attempt to start lisp after a watchdog reset, in case lisp
	// was the cause of it.
	// TODO: Anything else to check?
	if (!timeout_had_IWDG_reset() && terminal_get_first_fault() != FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET) {
		start_lisp(false);
	}
}

void ctx_cb(eval_context_t *ctx, void *arg1, void *arg2) {
	if (arg2 != NULL) {
		lbm_print_value((char*)arg2, 40, ctx->r);
	}

	if (arg1 != NULL) {
		float *res = (float*)arg1;
		*res = 100.0 * (float)ctx->K.max_sp / 256.0;
	}
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
			ok = start_lisp(true);
		}

		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = packet_id;
		send_buffer[ind++] = ok;
		reply_func(send_buffer, ind);
	} break;


	case COMM_LISP_GET_STATS: {
		lbm_heap_state_t heap_state;

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

		lbm_get_heap_state(&heap_state);
		if (heap_state.gc_num > 0) {
			heap_use = 100.0 * (float)(HEAP_SIZE - heap_state.gc_recovered) / (float)HEAP_SIZE;
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
		while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
			lbm_value key_val = lbm_car(curr);
			if (lbm_type_of(lbm_car(key_val)) == LBM_VAL_TYPE_SYMBOL && lbm_is_number(lbm_cdr(key_val))) {
				const char *name = lbm_get_name_by_symbol(lbm_dec_sym(lbm_car(key_val)));
				strcpy((char*)(send_buffer_global + ind), name);
				ind += strlen(name) + 1;
				buffer_append_float32_auto(send_buffer_global, lbm_dec_as_f(lbm_cdr(key_val)), &ind);
			}

			if (ind > 300) {
				break;
			}

			curr = lbm_cdr(curr);
		}

		reply_func(send_buffer_global, ind);
		chMtxUnlock(&send_buffer_mutex);
	} break;

	default:
		break;
	}
}

static bool start_lisp(bool print) {
	bool res = false;

	char *code_data = (char*)flash_helper_code_data(CODE_IND_LISP);
	int32_t code_len = flash_helper_code_size(CODE_IND_LISP);

	if (code_data != 0 && code_len > 0) {
		if (!lisp_thd_running) {
			lbm_init(heap, HEAP_SIZE, memory_array, LISP_MEM_SIZE, bitmap_array, LISP_MEM_BITMAP_SIZE);

			lbm_set_timestamp_us_callback(timestamp_callback);
			lbm_set_usleep_callback(sleep_callback);
			chThdCreateStatic(eval_thread_wa, sizeof(eval_thread_wa), NORMALPRIO, eval_thread, NULL);

			lisp_thd_running = true;
		} else {
			lbm_pause_eval();
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				chThdSleepMilliseconds(100);
			}

			lbm_init(heap, HEAP_SIZE, memory_array, LISP_MEM_SIZE, bitmap_array, LISP_MEM_BITMAP_SIZE);

			lbm_pause_eval();
			while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				chThdSleepMilliseconds(100);
			}
		}

		lispif_load_vesc_extensions();

		if (print) {
			commands_printf_lisp("Parsing %d characters", strlen(code_data));
		}

		lbm_create_char_stream_from_string(&string_tok_state, &string_tok, code_data);
		lbm_load_and_eval_program(&string_tok);
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
