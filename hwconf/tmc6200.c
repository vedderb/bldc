/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#include "conf_general.h"
#ifdef HW_HAS_TMC6200

#include "tmc6200.h"
#include "../tmc/ic/TMC6200/TMC6200.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils.h"
#include "terminal.h"
#include "commands.h"
#include <string.h>
#include <stdio.h>
#include "mc_interface.h"

// Private functions
static void spi_transfer(uint8_t *in_buf, const uint8_t *out_buf, int length);
static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);


// Private variables
static mutex_t m_spi_mutex;
static bool tmc6200_found = false;


void tmc6200_init(void) {
	chMtxObjectInit(&m_spi_mutex);

	// TMC6200 SPI
	palSetPadMode(TMC6200_MISO_GPIO, TMC6200_MISO_PIN, PAL_MODE_INPUT);
	palSetPadMode(TMC6200_SCK_GPIO, TMC6200_SCK_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(TMC6200_CS_GPIO, TMC6200_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(TMC6200_MOSI_GPIO, TMC6200_MOSI_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(TMC6200_MOSI_GPIO, TMC6200_MOSI_PIN);

	chThdSleepMilliseconds(100);

	// Disable OC
    // TODO

	//tmc6200_set_current_amp_gain(CURRENT_AMP_GAIN);

    // Get version number and enable driver if it matches
    int ic_version = (tmc6200_readInt(0, TMC6200_IOIN_OUTPUT) & TMC6200_VERSION_MASK) >> TMC6200_VERSION_SHIFT;
    tmc6200_found = ic_version == 0x10;
}


uint8_t tmc6200_readwriteByte(uint8_t motor, uint8_t data, uint8_t lastTransfer)
{
    spi_begin();
	uint8_t rx;
	spi_transfer(&rx, &data, 1);
    if (lastTransfer)
    {
        spi_end();
    }
    return rx;
}



static void spi_transfer(uint8_t *in_buf, const uint8_t *out_buf, int length) {
	for (int i = 0;i < length;i++) {
		uint8_t send = out_buf ? out_buf[i] : 0xFFFF;
		uint8_t recieve = 0;

		for (int bit = 0;bit < 8;bit++) {
			palWritePad(TMC6200_MOSI_GPIO, TMC6200_MOSI_PIN, send >> 7);
			send <<= 1;

			palSetPad(TMC6200_SCK_GPIO, TMC6200_SCK_PIN);
			spi_delay();

			palClearPad(TMC6200_SCK_GPIO, TMC6200_SCK_PIN);

			int r1, r2, r3;
			r1 = palReadPad(TMC6200_MISO_GPIO, TMC6200_MISO_PIN);
			__NOP();
			r2 = palReadPad(TMC6200_MISO_GPIO, TMC6200_MISO_PIN);
			__NOP();
			r3 = palReadPad(TMC6200_MISO_GPIO, TMC6200_MISO_PIN);

			recieve <<= 1;
			if (utils_middle_of_3_int(r1, r2, r3)) {
				recieve |= 1;
			}

			spi_delay();
		}

		if (in_buf) {
			in_buf[i] = recieve;
		}
	}
}

static void spi_begin(void) {
	palClearPad(TMC6200_CS_GPIO, TMC6200_CS_PIN);
}

static void spi_end(void) {
	palSetPad(TMC6200_CS_GPIO, TMC6200_CS_PIN);
}

static void spi_delay(void) {
	for (volatile int i = 0;i < 10;i++) {
		__NOP();
	}
}


#endif
