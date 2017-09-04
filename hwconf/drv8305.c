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

#include "hw.h"
#ifdef HW_HAS_DRV8305

#include "drv8305.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils.h"
#include <string.h>

// Private functions
static uint16_t spi_exchange(uint16_t x);
static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length);
static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);

// Private variables
static char m_fault_print_buffer[120];

void drv8305_init(void) {
	// DRV8305 SPI
	palSetPadMode(DRV8305_MISO_GPIO, DRV8305_MISO_PIN, PAL_MODE_INPUT);
	palSetPadMode(DRV8305_SCK_GPIO, DRV8305_SCK_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(DRV8305_CS_GPIO, DRV8305_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(DRV8305_MOSI_GPIO, DRV8305_MOSI_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(DRV8305_MOSI_GPIO, DRV8305_MOSI_PIN);

	chThdSleepMilliseconds(100);

	drv8305_read_reg(1);
}

/**
 * Read the fault codes of the DRV8305.
 *
 * @return
 * The fault codes, where the bits represent the following:
 * b0: FETLC_OC
 * b1: FETHC_OC
 * b2: FETLB_OC
 * b3: FETHB_OC
 * b4: FETLA_OC
 * b5: FETHA_OC
 * b6: OTW
 * b7: OTSD
 * b8: PVDD_UV
 * b9: GVDD_UV
 * b10: FAULT
 * b11: GVDD_OV
 *
 */
int drv8305_read_faults(void) {
	int r0 = drv8305_read_reg(0);
	int r1 = drv8305_read_reg(1);
	return r0 | (((r1 >> 7) & 0x01) << 4);
}

/**
 * Reset all latched faults.
 */
void drv8305_reset_faults(void) {
	int reg = drv8305_read_reg(2);
	reg |= 1 << 2;
	drv8305_write_reg(2, reg);
}

char* drv8305_faults_to_string(int faults) {
	if (faults == 0) {
		strcpy(m_fault_print_buffer, "No DRV8305 faults");
	} else {
		strcpy(m_fault_print_buffer, "|");

//		if (faults & DRV8305_FAULT_FETLC_OC) {
//			strcat(m_fault_print_buffer, " FETLC_OC |");
//		}
//
//		if (faults & DRV8305_FAULT_FETHC_OC) {
//			strcat(m_fault_print_buffer, " FETHC_OC |");
//		}
//
//		if (faults & DRV8305_FAULT_FETLB_OC) {
//			strcat(m_fault_print_buffer, " FETLB_OC |");
//		}
//
//		if (faults & DRV8305_FAULT_FETHB_OC) {
//			strcat(m_fault_print_buffer, " FETHB_OC |");
//		}
//
//		if (faults & DRV8305_FAULT_FETLA_OC) {
//			strcat(m_fault_print_buffer, " FETLA_OC |");
//		}
//
//		if (faults & DRV8305_FAULT_FETHA_OC) {
//			strcat(m_fault_print_buffer, " FETHA_OC |");
//		}
//
//		if (faults & DRV8305_FAULT_OTW) {
//			strcat(m_fault_print_buffer, " OTW |");
//		}
//
//		if (faults & DRV8305_FAULT_OTSD) {
//			strcat(m_fault_print_buffer, " OTSD |");
//		}
//
//		if (faults & DRV8305_FAULT_PVDD_UV) {
//			strcat(m_fault_print_buffer, " PVDD_UV |");
//		}
//
//		if (faults & DRV8305_FAULT_GVDD_UV) {
//			strcat(m_fault_print_buffer, " GVDD_UV |");
//		}
//
//		if (faults & DRV8305_FAULT_FAULT) {
//			strcat(m_fault_print_buffer, " FAULT |");
//		}
//
//		if (faults & DRV8305_FAULT_GVDD_OV) {
//			strcat(m_fault_print_buffer, " GVDD_OV |");
//		}
	}

	return m_fault_print_buffer;
}

unsigned int drv8305_read_reg(int reg) {
	uint16_t out = 0;
	out |= (1 << 15);
	out |= (reg & 0x0F) << 11;
	out |= 0x807F;

	if (reg != 0) {
		spi_begin();
		spi_exchange(out);
		spi_end();
	}

	spi_begin();
	uint16_t res = spi_exchange(0xFFFF);
	spi_end();

	return res;
}

void drv8305_write_reg(int reg, int data) {
	uint16_t out = 0;
	out |= (reg & 0x0F) << 11;
	out |= data & 0x7FF;

	spi_begin();
	spi_exchange(out);
	spi_end();
}

// Software SPI
static uint16_t spi_exchange(uint16_t x) {
	uint16_t rx;
	spi_transfer(&rx, &x, 1);
	return rx;
}

static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length) {
	for (int i = 0;i < length;i++) {
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
		uint16_t recieve = 0;

		for (int bit = 0;bit < 16;bit++) {
			palWritePad(DRV8305_MOSI_GPIO, DRV8305_MOSI_PIN, send >> 15);
			send <<= 1;

			palSetPad(DRV8305_SCK_GPIO, DRV8305_SCK_PIN);
			spi_delay();

			palClearPad(DRV8305_SCK_GPIO, DRV8305_SCK_PIN);

			int r1, r2, r3;
			r1 = palReadPad(DRV8305_MISO_GPIO, DRV8305_MISO_PIN);
			__NOP();
			r2 = palReadPad(DRV8305_MISO_GPIO, DRV8305_MISO_PIN);
			__NOP();
			r3 = palReadPad(DRV8305_MISO_GPIO, DRV8305_MISO_PIN);

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
	palClearPad(DRV8305_CS_GPIO, DRV8305_CS_PIN);
}

static void spi_end(void) {
	palSetPad(DRV8305_CS_GPIO, DRV8305_CS_PIN);
}

static void spi_delay(void) {
	for (volatile int i = 0;i < 10;i++) {
		__NOP();
	}
}

#endif
