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
#include "hw.h"
#include "mc_interface.h"
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

static bool get_gpio(VESC_PIN io, stm32_gpio_t **port, uint32_t *pin, bool *is_analog) {
	bool res = false;
	*is_analog = false;

	switch (io) {
	case VESC_PIN_COMM_RX:
#ifdef HW_UART_RX_PORT
		*port = HW_UART_RX_PORT; *pin = HW_UART_RX_PIN;
		res = true;
#endif
		break;
	case VESC_PIN_COMM_TX:
#ifdef HW_UART_TX_PORT
		*port = HW_UART_TX_PORT; *pin = HW_UART_TX_PIN;
		res = true;
#endif
		break;
	case VESC_PIN_SWDIO:
		*port = GPIOA; *pin = 13;
		res = true;
		break;
	case VESC_PIN_SWCLK:
		*port = GPIOA; *pin = 14;
		res = true;
		break;
	case VESC_PIN_HALL1:
		*port = HW_HALL_ENC_GPIO1; *pin = HW_HALL_ENC_PIN1;
		res = true;
		break;
	case VESC_PIN_HALL2:
		*port = HW_HALL_ENC_GPIO2; *pin = HW_HALL_ENC_PIN2;
		res = true;
		break;
	case VESC_PIN_HALL3:
		*port = HW_HALL_ENC_GPIO3; *pin = HW_HALL_ENC_PIN3;
		res = true;
		break;
	case VESC_PIN_ADC1:
#ifdef HW_ADC_EXT_GPIO
		*port = HW_ADC_EXT_GPIO; *pin = HW_ADC_EXT_PIN;
		*is_analog = true;
		res = true;
#endif
		break;
	case VESC_PIN_ADC2:
#ifdef HW_ADC_EXT2_GPIO
		*port = HW_ADC_EXT2_GPIO; *pin = HW_ADC_EXT2_PIN;
		*is_analog = true;
		res = true;
#endif
		break;
	}

	return res;
}

static bool lib_io_set_mode(VESC_PIN pin_vesc, VESC_PIN_MODE mode) {
	stm32_gpio_t *gpio;
	uint32_t pin;
	bool is_analog;
	bool res = false;

	if (get_gpio(pin_vesc, &gpio, &pin, &is_analog)) {
		switch (mode) {
		case VESC_PIN_MODE_INPUT_NOPULL:
			palSetPadMode(gpio, pin, PAL_MODE_INPUT);
			res = true;
			break;
		case VESC_PIN_MODE_INPUT_PULL_UP:
			palSetPadMode(gpio, pin, PAL_MODE_INPUT_PULLUP);
			res = true;
			break;
		case VESC_PIN_MODE_INPUT_PULL_DOWN:
			palSetPadMode(gpio, pin, PAL_MODE_INPUT_PULLDOWN);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_PUSHPULL);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT_OPEN_DRAIN:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_OPENDRAIN);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT_OPEN_DRAIN_PULL_UP:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLUP);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT_OPEN_DRAIN_PULL_DOWN:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLDOWN);
			res = true;
			break;
		case VESC_PIN_MODE_ANALOG:
			if (is_analog) {
				palSetPadMode(gpio, pin, PAL_STM32_MODE_ANALOG);
				res = true;
			}
			break;
		}
	}

	return res;
}

static bool lib_io_write(VESC_PIN pin_vesc, int state) {
	stm32_gpio_t *gpio;
	uint32_t pin;
	bool is_analog;
	bool res = false;

	if (get_gpio(pin_vesc, &gpio, &pin, &is_analog)) {
		palWritePad(gpio, pin, state);
		res = true;
	}

	return res;
}

static bool lib_io_read(VESC_PIN pin_vesc) {
	stm32_gpio_t *gpio;
	uint32_t pin;
	bool is_analog;
	bool res = false;

	if (get_gpio(pin_vesc, &gpio, &pin, &is_analog)) {
		res = palReadPad(gpio, pin);
	}

	return res;
}

static float lib_io_read_analog(VESC_PIN pin_vesc) {
	float res = -1.0;

	if (pin_vesc == VESC_PIN_ADC1) {
		res = ADC_VOLTS(ADC_IND_EXT);
	} else if (pin_vesc == VESC_PIN_ADC2) {
		res = ADC_VOLTS(ADC_IND_EXT2);
	}

	return res;
}

static bool lib_io_get_st_pin(VESC_PIN vesc_pin, void **gpio, uint32_t *pin) {
	bool analog;
	return get_gpio(vesc_pin, (stm32_gpio_t**)gpio, pin, &analog);
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

		// ST IO
		cif.cif.set_pad_mode = lib_set_pad_mode;
		cif.cif.set_pad = lib_set_pad;
		cif.cif.clear_pad = lib_clear_pad;

		// Abstract IO
		cif.cif.io_set_mode = lib_io_set_mode;
		cif.cif.io_write = lib_io_write;
		cif.cif.io_read = lib_io_read;
		cif.cif.io_read_analog = lib_io_read_analog;
		cif.cif.io_get_st_pin = lib_io_get_st_pin;

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

			if (loaded_libs[i].stop_fun != NULL && (uint32_t)loaded_libs[i].stop_fun < 0x10000000) {
				loaded_libs[i].stop_fun = NULL;
				lbm_set_error_reason("Invalid stop function. Make sure that it is static.");
				return res;
			}

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
