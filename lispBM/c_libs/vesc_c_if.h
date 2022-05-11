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

typedef struct {
	// LBM If
	load_extension_fptr lbm_add_extension;
	float (*lbm_dec_as_float)(lbm_value);
	uint32_t (*lbm_dec_as_u32)(lbm_value);
	int32_t (*lbm_dec_as_i32)(lbm_value);
	lbm_value (*lbm_enc_float)(float);
	lbm_value (*lbm_enc_u32)(uint32_t);
	lbm_value (*lbm_enc_i32)(int32_t);
	lbm_value (*lbm_cons)(lbm_value, lbm_value);
	lbm_value (*lbm_car)(lbm_value);
	lbm_value (*lbm_cdr)(lbm_value);
	bool (*lbm_is_array)(lbm_value);
	
	// Os
	void (*sleep_ms)(uint32_t);
	void (*sleep_us)(uint32_t);
	int (*printf)(const char*, ...);
	void* (*malloc)(size_t);
	void (*free)(void*);
	lib_thread (*spawn)(void (*)(void*),size_t,char*,void*);
	void (*request_terminate)(lib_thread);
	bool (*should_terminate)(void);
} vesc_c_if;

typedef struct {
	void (*stop_fun)(void *arg);
	void *arg;
	uint32_t base_addr;
} lib_info;

#define VESC_IF		((vesc_c_if*)(0x1000FC00))
#define INIT_FUN	bool __attribute__((__section__(".init_fun"))) init

#endif  // VESC_C_IF_H

