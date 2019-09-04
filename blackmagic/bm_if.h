/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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

#ifndef BLACKMAGIC_BM_IF_H_
#define BLACKMAGIC_BM_IF_H_

#include <stdint.h>
#include <stdbool.h>
#include "hal.h"

void bm_init(void);
void bm_set_enabled(bool enabled);
int bm_connect(void);
int bm_erase_flash_all(void);
int bm_write_flash(uint32_t addr, const void *data, uint32_t len);
int bm_mem_read(uint32_t addr, void *data, uint32_t len);
int bm_reboot(void);
void bm_leave_nrf_debug_mode(void);
void bm_disconnect(void);
void bm_change_swd_pins(stm32_gpio_t *swdio_port, int swdio_pin,
		stm32_gpio_t *swclk_port, int swclk_pin);
void bm_default_swd_pins(void);

#endif /* BLACKMAGIC_BM_IF_H_ */
