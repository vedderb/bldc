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

/*
 * Observed issues:
 * * [Fixed] Dividing an integer by a float causes eval_error
 * * extern keyword should be removed for functions as is makes no difference
 */

#define HEAP_SIZE				1024
#define LISP_MEM_SIZE			MEMORY_SIZE_4K
#define LISP_MEM_BITMAP_SIZE	MEMORY_BITMAP_SIZE_4K

__attribute__((section(".ram4"))) static cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
__attribute__((section(".ram4"))) static uint32_t memory_array[LISP_MEM_SIZE];
__attribute__((section(".ram4"))) static uint32_t bitmap_array[LISP_MEM_BITMAP_SIZE];

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
	eval_cps_run_eval();
}

static void terminal_start(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	char *code = (char*)(0x08060000);

	if (!lisp_thd_running) {
		lispbm_init(heap, HEAP_SIZE, memory_array, LISP_MEM_SIZE, bitmap_array, LISP_MEM_BITMAP_SIZE);

		eval_cps_set_timestamp_us_callback(timestamp_callback);
		eval_cps_set_usleep_callback(sleep_callback);
		chThdCreateStatic(eval_thread_wa, sizeof(eval_thread_wa), NORMALPRIO, eval_thread, NULL);

		lisp_thd_running = true;
	} else {
		eval_cps_pause_eval();
		while (eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
			chThdSleepMilliseconds(100);
		}

		lispbm_init(heap, HEAP_SIZE, memory_array, LISP_MEM_SIZE, bitmap_array, LISP_MEM_BITMAP_SIZE);

		eval_cps_pause_eval();
		while (eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
			chThdSleepMilliseconds(100);
		}
	}

	lispif_load_vesc_extensions();

	VALUE t = tokpar_parse(code);

	eval_cps_program(t);
	eval_cps_continue_eval();

	commands_printf("Lisp started");
}

static void terminal_stop(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	eval_cps_pause_eval();
	while (eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
		chThdSleepMilliseconds(100);
	}
}

void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
	(void)arg2;
	char outbuf[256];
	print_value(outbuf, 256, ctx->r);
	commands_printf("%s %x %u %u %s", (char*)arg1, (uint32_t)ctx, ctx->id, ctx->K.sp, outbuf );
	commands_printf("Stack SP max: %u (Limit: 256)\n", ctx->K.max_sp);
}


static void terminal_stats(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	char outbuf[256];

	heap_state_t heap_state;

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
	commands_printf("Used cons cells: %lu", HEAP_SIZE - heap_num_free());
	commands_printf("Free cons cells: %lu", heap_num_free());
	heap_get_state(&heap_state);
	commands_printf("GC counter: %lu", heap_state.gc_num);
	commands_printf("Recovered: %lu", heap_state.gc_recovered);
	commands_printf("Marked: %lu", heap_state.gc_marked);

	commands_printf("Array and symbol string memory:");
	commands_printf("  Size: %u 32Bit words", memory_num_words());
	commands_printf("  Free: %u 32Bit words", memory_num_free());
	commands_printf("------------------------------------------------------------");

	VALUE curr = *env_get_global_ptr();
	commands_printf("Environment:");

	while (type_of(curr) == PTR_TYPE_CONS) {
		print_value(outbuf, 256, car(curr));
	    curr = cdr(curr);
	    commands_printf("  %s", outbuf);
    }

	commands_printf("Runnable:");
	eval_cps_running_iterator(print_ctx_info, "RUNNABLE", NULL);
	commands_printf("Blocked:");
	eval_cps_blocked_iterator(print_ctx_info, "BLOCKED", NULL);
	commands_printf("Done:");
	eval_cps_done_iterator(print_ctx_info, "DONE", NULL);

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
