/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#include "ch.h"
#include "hal.h"
#include "commands.h"
#include "lispif.h"
#include "lispbm.h"
#include "c_libs/vesc_c_if.h"

typedef struct {
	char *name;
	void *arg;
	void (*func)(void*);
	void *w_mem;
} lib_thd_info;

#define LIB_NUM_MAX		10
static lib_info loaded_libs[LIB_NUM_MAX] = {0};

static bool lib_init_done = false;

__attribute__((section(".libif"))) static volatile union {
	vesc_c_if cif;
	char pad[1024];
} cif;

static void lib_sleep_ms(uint32_t ms) {
	chThdSleepMilliseconds(ms);
}

static void lib_sleep_us(uint32_t us) {
	chThdSleepMicroseconds(us);
}

static void* lib_malloc(size_t size) {
	lbm_uint alloc_size;
	if (size % sizeof(lbm_uint) == 0) {
		alloc_size = size / (sizeof(lbm_uint));
	} else {
		alloc_size = (size / (sizeof(lbm_uint))) + 1;
	}

	return lbm_memory_allocate(alloc_size);
}

static void lib_free(void *ptr) {
	lbm_memory_free(ptr);
}

static THD_FUNCTION(lib_thd, arg) {
	lib_thd_info *t = (lib_thd_info*)arg;
	chRegSetThreadName(t->name);
	t->func(t->arg);
	lib_free(t->w_mem);
	lib_free(t);
}

static lib_thread lib_spawn(void (*func)(void*), size_t stack_size, char *name, void *arg) {
	void *mem = lib_malloc(stack_size);

	if (mem) {
		lib_thd_info *info = lib_malloc(sizeof(lib_thd_info));

		if (info) {
			info->arg = arg;
			info->func = func;
			info->name = name;
			info->w_mem = mem;
			return (lib_thread)chThdCreateStatic(mem, stack_size, NORMALPRIO, lib_thd, info);
		}
	}

	return 0;
}

static void lib_request_terminate(lib_thread thd) {
	chThdTerminate((thread_t*)thd);
	chThdWait((thread_t*)thd);
}

static bool lib_should_terminate(void) {
	return chThdShouldTerminateX();
}

static void** lib_get_arg(uint32_t prog_addr) {
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].base_addr == prog_addr) {
			return &loaded_libs[i].arg;
		}
	}
	return 0;
}

static void lib_set_pad_mode(void *gpio, uint32_t pin, uint32_t mode) {
	palSetPadMode((stm32_gpio_t*)gpio, pin, mode);
}

static void lib_set_pad(void *gpio, uint32_t pin) {
	palSetPad((stm32_gpio_t*)gpio, pin);
}

static void lib_clear_pad(void *gpio, uint32_t pin) {
	palClearPad((stm32_gpio_t*)gpio, pin);
}

lbm_value ext_load_native_lib(lbm_value *args, lbm_uint argn) {
	lbm_value res = lbm_enc_sym(SYM_EERROR);

	if (argn != 1 || !lbm_is_array(args[0])) {
		return res;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	if (array->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	if (!lib_init_done) {
		memset((char*)cif.pad, 0, 1024);

		// LBM
		cif.cif.lbm_add_extension = lbm_add_extension;
		cif.cif.lbm_dec_as_float = lbm_dec_as_float;
		cif.cif.lbm_dec_as_u32 =	lbm_dec_as_u32;
		cif.cif.lbm_dec_as_i32 = lbm_dec_as_i32;
		cif.cif.lbm_enc_float = lbm_enc_float;
		cif.cif.lbm_enc_u32 = lbm_enc_u32;
		cif.cif.lbm_enc_i32 = lbm_enc_i32;
		cif.cif.lbm_cons = lbm_cons;
		cif.cif.lbm_car = lbm_car;
		cif.cif.lbm_cdr = lbm_cdr;
		cif.cif.lbm_is_array = lbm_is_array;
		cif.cif.lbm_set_error_reason = lbm_set_error_reason;

		// Os
		cif.cif.sleep_ms = lib_sleep_ms;
		cif.cif.sleep_us = lib_sleep_us;
		cif.cif.printf = commands_printf_lisp;
		cif.cif.malloc = lib_malloc;
		cif.cif.free = lib_free;
		cif.cif.spawn = lib_spawn;
		cif.cif.request_terminate = lib_request_terminate;
		cif.cif.should_terminate = lib_should_terminate;
		cif.cif.get_arg = lib_get_arg;

		// IO
		cif.cif.set_pad_mode = lib_set_pad_mode;
		cif.cif.set_pad = lib_set_pad;
		cif.cif.clear_pad = lib_clear_pad;

		lib_init_done = true;
	}

	uint32_t addr = (uint32_t)array->data;

	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun != NULL && loaded_libs[i].base_addr == addr) {
			lbm_set_error_reason("Library already loaded");
			return res;
		}
	}

	bool ok = false;
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun == NULL) {
			loaded_libs[i].base_addr = addr;
			addr += 4; // Skip program pointer
			addr |= 1; // Ensure that thumb mode is used (??)
			ok = ((bool(*)(lib_info *info))addr)(&loaded_libs[i]);
			break;
		}
	}

	if (ok) {
		res = lbm_enc_sym(SYM_TRUE);
	} else {
		lbm_set_error_reason("Library init failed");
	}

	return res;
}

lbm_value ext_unload_native_lib(lbm_value *args, lbm_uint argn) {
	lbm_value res = lbm_enc_sym(SYM_EERROR);

	if (argn != 1 || !lbm_is_array(args[0])) {
		return res;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	if (array->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	uint32_t addr = (uint32_t)array->data;

	bool ok = false;
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun != NULL && loaded_libs[i].base_addr == addr) {
			loaded_libs[i].stop_fun(loaded_libs[i].arg);
			loaded_libs[i].stop_fun = NULL;
			res = lbm_enc_sym(SYM_TRUE);
			ok = true;
		}
	}

	if (!ok) {
		lbm_set_error_reason("Library not loaded");
	}

	return res;
}

void lispif_stop_lib(void) {
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun != NULL) {
			loaded_libs[i].stop_fun(loaded_libs[i].arg);
			loaded_libs[i].stop_fun = NULL;
		}
	}
}
