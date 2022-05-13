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

#ifndef VESC_C_IF_H
#define VESC_C_IF_H

#include <stdint.h>
#include <stdbool.h>

typedef bool (*load_extension_fptr)(char*,extension_fptr);
typedef void* lib_thread;

typedef enum {
	VESC_PIN_COMM_RX = 0,
	VESC_PIN_COMM_TX,
	VESC_PIN_SWDIO,
	VESC_PIN_SWCLK,
	VESC_PIN_HALL1,
	VESC_PIN_HALL2,
	VESC_PIN_HALL3,
	VESC_PIN_ADC1,
	VESC_PIN_ADC2,
} VESC_PIN;

typedef enum {
	VESC_PIN_MODE_INPUT_NOPULL = 0,
	VESC_PIN_MODE_INPUT_PULL_UP,
	VESC_PIN_MODE_INPUT_PULL_DOWN,
	VESC_PIN_MODE_OUTPUT,
	VESC_PIN_MODE_OUTPUT_OPEN_DRAIN,
	VESC_PIN_MODE_OUTPUT_OPEN_DRAIN_PULL_UP,
	VESC_PIN_MODE_OUTPUT_OPEN_DRAIN_PULL_DOWN,
	VESC_PIN_MODE_ANALOG,
} VESC_PIN_MODE;

typedef struct {
	// LBM If
	load_extension_fptr lbm_add_extension;
	float (*lbm_dec_as_float)(lbm_value val);
	uint32_t (*lbm_dec_as_u32)(lbm_value val);
	int32_t (*lbm_dec_as_i32)(lbm_value val);
	lbm_value (*lbm_enc_float)(float f);
	lbm_value (*lbm_enc_u32)(uint32_t u);
	lbm_value (*lbm_enc_i32)(int32_t i);
	lbm_value (*lbm_cons)(lbm_value car, lbm_value cdr);
	lbm_value (*lbm_car)(lbm_value val);
	lbm_value (*lbm_cdr)(lbm_value val);
	bool (*lbm_is_array)(lbm_value val);
	int (*lbm_set_error_reason)(char *str);
	
	// Os
	void (*sleep_ms)(uint32_t ms);
	void (*sleep_us)(uint32_t us);
	int (*printf)(const char *str, ...);
	void* (*malloc)(size_t bytes);
	void (*free)(void *prt);
	lib_thread (*spawn)(void (*fun)(void *arg), size_t stack_size, char *name, void *arg);
	void (*request_terminate)(lib_thread thd);
	bool (*should_terminate)(void);
	void** (*get_arg)(uint32_t prog_addr);
	
	// ST IO
	void (*set_pad_mode)(void *gpio, uint32_t pin, uint32_t mode);
	void (*set_pad)(void *gpio, uint32_t pin);
	void (*clear_pad)(void *gpio, uint32_t pin);
	
	// Abstract IO
	bool (*io_set_mode)(VESC_PIN pin, VESC_PIN_MODE mode);
	bool (*io_write)(VESC_PIN pin, int state);
	bool (*io_read)(VESC_PIN pin);
	float (*io_read_analog)(VESC_PIN pin);
	bool (*io_get_st_pin)(VESC_PIN vesc_pin, void **gpio, uint32_t *pin);
} vesc_c_if;

typedef struct {
	void (*stop_fun)(void *arg);
	void *arg;
	uint32_t base_addr;
} lib_info;

// VESC-interface with function pointers
#define VESC_IF		((vesc_c_if*)(0x1000FC00))

// Put this at the beginning of your source file
#define HEADER		static volatile int __attribute__((__section__(".program_ptr"))) prog_ptr;

// Init function
#define INIT_FUN	bool __attribute__((__section__(".init_fun"))) init

// Put this at the start of the init function
#define INIT_START	(void)prog_ptr;

// Address of this program in memory
#define PROG_ADDR	((uint32_t)&prog_ptr)

// The argument that was set in the init function (same as the one you get in stop_fun)
#define ARG			(*VESC_IF->get_arg(PROG_ADDR))

#endif  // VESC_C_IF_H

