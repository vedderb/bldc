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

#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "lispbm_memory.h"
#include "env.h"
#include "lispbm.h"

#define HEAP_SIZE				1024
#define LISP_MEM_SIZE			LBM_MEMORY_SIZE_8K
#define LISP_MEM_BITMAP_SIZE	LBM_MEMORY_BITMAP_SIZE_8K

__attribute__((section(".ram4"))) static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static uint32_t memory_array[LISP_MEM_SIZE];
static uint32_t bitmap_array[LISP_MEM_BITMAP_SIZE];
static lbm_tokenizer_string_state_t string_tok_state;
static lbm_tokenizer_char_stream_t string_tok;

static thread_t *eval_tp = 0;
static THD_WORKING_AREA(eval_thread_wa, 2048);
static bool lisp_thd_running = false;

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

static void terminal_start(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	char *code = (char*)(0x080A0000);

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

	commands_printf("Parsing %d characters", strlen(code));

	lbm_create_char_stream_from_string(&string_tok_state, &string_tok, code);
	lbm_load_and_eval_program(&string_tok);
	lbm_continue_eval();

//	VALUE t = tokpar_parse(code);
//
//	if (dec_sym(t) == SYM_STACK_ERROR) {
//		commands_printf("Lisp parser ran out of stack");
//	} else if (dec_sym(t) == SYM_RERROR) {
//		commands_printf("Lisp parser error");
//	} else {
//		eval_cps_program(t);
//		eval_cps_continue_eval();
//		commands_printf("Lisp started");
//	}
}

static void terminal_stop(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	lbm_pause_eval();
	while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
		chThdSleepMilliseconds(100);
	}
}

void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
	(void)arg2;
	char outbuf[256];
	lbm_print_value(outbuf, 256, ctx->r);
	commands_printf("%s %x %u %u %s", (char*)arg1, (uint32_t)ctx, ctx->id, ctx->K.sp, outbuf );
	commands_printf("Stack SP max: %u (Limit: 256)\n", ctx->K.max_sp);
}


static void terminal_stats(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	char outbuf[256];

	lbm_heap_state_t heap_state;

	static systime_t time_last = 0;
	if (eval_tp) {
		commands_printf("Eval CPU time: %.2f %%", (double)(100.0 * eval_tp->p_time / (chVTGetSystemTimeX() - time_last)));
		time_last = chVTGetSystemTimeX();
		eval_tp->p_time = 0;
	} else {
		commands_printf("Not running\n");
		return;
	}

	commands_printf("------------------------------------------------------------\r\n");
	commands_printf("Used cons cells: %lu", HEAP_SIZE - lbm_heap_num_free());
	commands_printf("Free cons cells: %lu", lbm_heap_num_free());
	lbm_get_heap_state(&heap_state);
	commands_printf("GC counter: %lu", heap_state.gc_num);
	commands_printf("Recovered: %lu", heap_state.gc_recovered);
	commands_printf("Marked: %lu", heap_state.gc_marked);

	commands_printf("Array and symbol string memory:");
	commands_printf("  Size: %u 32Bit words", lbm_memory_num_words());
	commands_printf("  Free: %u 32Bit words", lbm_memory_num_free());
	commands_printf("------------------------------------------------------------");

	lbm_value curr = *lbm_get_env_ptr();
	commands_printf("Environment:");
	while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
		lbm_print_value(outbuf, 256, lbm_car(curr));
	    curr = lbm_cdr(curr);
	    commands_printf("  %s", outbuf);
    }

	curr = *lbm_get_env_ptr();
	commands_printf("\"Number bindings\":");
	while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
		lbm_value key_val = lbm_car(curr);
		if (lbm_type_of(lbm_car(key_val)) == LBM_VAL_TYPE_SYMBOL && lbm_is_number(lbm_cdr(key_val))) {
			const char *name = lbm_get_name_by_symbol(lbm_dec_sym(lbm_car(key_val)));
			commands_printf("  Name: %s Val: %f", name, (double)lbm_dec_as_f(lbm_cdr(key_val)));
		}
		curr = lbm_cdr(curr);
	}

	commands_printf("Runnable:");
	lbm_running_iterator(print_ctx_info, "RUNNABLE", NULL);
	commands_printf("Blocked:");
	lbm_blocked_iterator(print_ctx_info, "BLOCKED", NULL);
	commands_printf("Done:");
	lbm_done_iterator(print_ctx_info, "DONE", NULL);

	commands_printf(" ");
}

void lispif_init(void) {
	terminal_register_command_callback(
			"lisp_run",
			"Run Lisp",
			0,
			terminal_start);

	terminal_register_command_callback(
			"lisp_stop",
			"Stop Lisp",
			0,
			terminal_stop);

	terminal_register_command_callback(
			"lisp_stats",
			"Print lisp stats",
			0,
			terminal_stats);
}
