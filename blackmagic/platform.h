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

#ifndef BLACKMAGIC_PLATFORM_H_
#define BLACKMAGIC_PLATFORM_H_

#include "ch.h"
#include "hal.h"
#include "timing.h"
#include "commands.h"

// Global variables
extern stm32_gpio_t *platform_swdio_port;
extern int platform_swdio_pin;
extern stm32_gpio_t *platform_swclk_port;
extern int platform_swclk_pin;

#define SWDIO_PORT_DEFAULT		GPIOA
#define SWDIO_PIN_DEFAULT		13
#define SWCLK_PORT_DEFAULT	 	GPIOA
#define SWCLK_PIN_DEFAULT		14

#define PLATFORM_HAS_DEBUG
#define DEBUG commands_printf

#define SWDIO_PORT 	platform_swdio_port
#define SWDIO_PIN	platform_swdio_pin
#define SWCLK_PORT 	platform_swclk_port
#define SWCLK_PIN	platform_swclk_pin

#define gpio_set(port, pin)				palSetPad(port, pin);palSetPad(port, pin);palSetPad(port, pin)
#define gpio_clear(port, pin)			palClearPad(port, pin);palClearPad(port, pin);palClearPad(port, pin)
#define gpio_set_val(port, pin, val)	palWritePad(port, pin, (val) ? PAL_HIGH : PAL_LOW);palWritePad(port, pin, val ? PAL_HIGH : PAL_LOW);palWritePad(port, pin, val ? PAL_HIGH : PAL_LOW)
#define gpio_get(port, pin)				palReadPad(port, pin)

#define SWDIO_MODE_FLOAT()				palSetPadMode(SWDIO_PORT, SWDIO_PIN, PAL_MODE_INPUT)
#define SWDIO_MODE_DRIVE()				palSetPadMode(SWDIO_PORT, SWDIO_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST)

// Avoid collisions with chibios definitions
#undef SRAM_BASE
#undef FLASH_CR_PG
#undef FLASH_CR_SER
#undef FLASH_CR_MER
#undef FLASH_CR_STRT
#undef FLASH_CR_EOPIE
#undef FLASH_CR_LOCK
#undef FLASH_SR_BSY
#undef FLASH_OPTCR_OPTLOCK
#undef FLASH_OPTCR_OPTSTRT
#undef FLASH_SR_EOP
#undef FLASH_SR_WRPERR
#undef FLASH_SR_PGSERR
#undef FLASH_CR_SNB_1
#undef FLASH_CR_SNB
#undef DBGMCU_CR_DBG_SLEEP
#undef DBGMCU_CR_DBG_STOP
#undef DBGMCU_CR_DBG_STANDBY
#undef FLASH_SR_PGAERR

#endif /* BLACKMAGIC_PLATFORM_H_ */
