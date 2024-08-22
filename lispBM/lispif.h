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

#ifndef LISPBM_LISPIF_H_
#define LISPBM_LISPIF_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "lispbm.h"
#include "hal.h"

typedef void* lib_thread;

// Functions
void lispif_init(void);
int lispif_get_restart_cnt(void);
void lispif_lock_lbm(void);
void lispif_unlock_lbm(void);
bool lispif_restart(bool print, bool load_code, bool load_imports);
void lispif_add_ext_load_callback(void (*p_func)(void));
void lispif_disable_all_events(void);
bool lispif_symbol_to_io(lbm_uint sym, stm32_gpio_t **port, uint32_t *pin);
void lispif_stop_lib(void);
float lispif_get_ppm(void);
void lispif_process_cmd(unsigned char *data, unsigned int len,
		void(*reply_func)(unsigned char *data, unsigned int len));
void lispif_process_can(uint32_t can_id, uint8_t *data8, int len, bool is_ext);
void lispif_process_custom_app_data(unsigned char *data, unsigned int len);
void lispif_process_shutdown(void);
void lispif_process_rmsg(int slot, unsigned char *data, unsigned int len);

void lispif_load_vesc_extensions(void);
bool lispif_vesc_dynamic_loader(const char *str, const char **code);
lib_thread lispif_spawn(void (*func)(void*), size_t stack_size, char *name, void *arg);

#endif /* LISPBM_LISPIF_H_ */
