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
static void terminal_read_reg(int argc, const char **argv);
static void terminal_write_reg(int argc, const char **argv);
static void terminal_tmc_6200_found(int argc, const char **argv);
static void terminal_read_currents(int argc, const char **argv);

// Private variables
static mutex_t m_spi_mutex;
static bool tmc6200_found = false;

bool tmc6200_ok(void) {
    return tmc6200_found;
}

void tmc6200_init(void) {
	chMtxObjectInit(&m_spi_mutex);

	// TMC6200 SPI
	palSetPadMode(TMC6200_MISO_GPIO, TMC6200_MISO_PIN, PAL_MODE_INPUT);
	palSetPadMode(TMC6200_SCK_GPIO, TMC6200_SCK_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(TMC6200_CS_GPIO, TMC6200_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(TMC6200_MOSI_GPIO, TMC6200_MOSI_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(TMC6200_MOSI_GPIO, TMC6200_MOSI_PIN);

	chThdSleepMilliseconds(100);



    terminal_register_command_callback(
            "tmc6200_read_reg",
            "Read a register from the TMC6200 and print it.",
            "[reg]",
            terminal_read_reg);

    terminal_register_command_callback(
            "tmc6200_write_reg",
            "Write to a TMC6200 register.",
            "[reg] [hexvalue]",
            terminal_write_reg);
    terminal_register_command_callback(
            "tmc6200_found",
            "Check if TMC was found on startup",
            "",
            terminal_tmc_6200_found);

    terminal_register_command_callback(
            "tmc6200_get_currents",
            "Read currents",
            "",
            terminal_read_currents);



	// Disable OC

	//tmc6200_set_current_amp_gain(CURRENT_AMP_GAIN);

    // Get version number and enable driver if it matches
    int ic_version = (tmc6200_readInt(0, TMC6200_IOIN_OUTPUT) & TMC6200_VERSION_MASK) >> TMC6200_VERSION_SHIFT;
    tmc6200_found = ic_version == 0x10;
    
    if(tmc6200_found) {
        tmc6200_write_conf();
    }
}


static void terminal_read_currents(int argc, const char **argv) {
    // Avoid unused variable warnings
    (void)(argc);
    (void)(argv);

    commands_printf("volts: %d, %d, %d", ADC_V_L1, ADC_V_L2, ADC_V_L3);
    commands_printf("currents: %d, %d, %d", GET_CURRENT1(), GET_CURRENT2(), GET_CURRENT3());
}

static void terminal_tmc_6200_found(int argc, const char **argv) {
    // Avoid unused variable warnings
    (void)(argc);
    (void)(argv);

    if(tmc6200_found) {
        commands_printf("TMC6200 found");
    } else {
        commands_printf("TMC6200 not found");
    }
}

static void terminal_read_reg(int argc, const char **argv) {
    if (argc == 2) {
        int reg = -1;
        sscanf(argv[1], "%d", &reg);

        if (reg >= 0) {
            uint32_t res = tmc6200_readInt(0,reg);
            char b1[9];
            char b2[9];
            char b3[9];
            char b4[9];

            utils_byte_to_binary((res >> 24) & 0xFF, b1);
            utils_byte_to_binary((res >> 16) & 0xFF, b2);
            utils_byte_to_binary((res >> 8) & 0xFF, b3);
            utils_byte_to_binary((res) & 0xFF, b4);

            commands_printf("Reg 0x%02x: %s %s %s %s (0x%04x)\n", reg, b1, b2, b3, b4, res);
        } else {
            commands_printf("Invalid argument(s).\n");
        }
    } else {
        commands_printf("This command requires one argument.\n");
    }
}

static void terminal_write_reg(int argc, const char **argv) {
    if (argc == 3) {
        int reg = -1;
        int val = -1;
        sscanf(argv[1], "%d", &reg);
        sscanf(argv[2], "%x", &val);

        if (reg >= 0 && val >= 0) {
            tmc6200_writeInt(0, reg, val);
            uint32_t res = tmc6200_readInt(0,reg);

            char b1[9];
            char b2[9];
            char b3[9];
            char b4[9];

            utils_byte_to_binary((res >> 24) & 0xFF, b1);
            utils_byte_to_binary((res >> 16) & 0xFF, b2);
            utils_byte_to_binary((res >> 8) & 0xFF, b3);
            utils_byte_to_binary((res) & 0xFF, b4);

            commands_printf("Reg 0x%02x: %s %s %s %s (0x%04x)\n", reg, b1, b2, b3, b4, res);
        } else {
            commands_printf("Invalid argument(s).\n");
        }
    } else {
        commands_printf("This command requires two arguments.\n");
    }
}


uint8_t tmc6200_readwriteByte(uint8_t motor, uint8_t data, uint8_t lastTransfer)
{
    // Avoid unused variable warnings. We only have one gate driver, therefore we don't need to read the motor parameter.
    (void)(motor);

    spi_begin();
	uint8_t rx = 0;
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

			palClearPad(TMC6200_SCK_GPIO, TMC6200_SCK_PIN);
			spi_delay();

			palSetPad(TMC6200_SCK_GPIO, TMC6200_SCK_PIN);

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
    palSetPad(TMC6200_SCK_GPIO, TMC6200_SCK_PIN);
	palClearPad(TMC6200_CS_GPIO, TMC6200_CS_PIN);
    spi_delay();
}

static void spi_end(void) {
    spi_delay();
    palSetPad(TMC6200_CS_GPIO, TMC6200_CS_PIN);
}

static void spi_delay(void) {
	for (volatile int i = 0;i < 10;i++) {
		__NOP();
	}
}

//int tmc6200_read_faults(void) {
//    return 1;
//}


void tmc6200_reset_faults(void) {
    // stay in error, if SPI comms are dead
    int ic_version = (tmc6200_readInt(0, TMC6200_IOIN_OUTPUT) & TMC6200_VERSION_MASK) >> TMC6200_VERSION_SHIFT;
    tmc6200_found = ic_version == 0x10;

    if(!tmc6200_found)
        return;
    DISABLE_GATE();
    chThdSleepMilliseconds(100);
    ENABLE_GATE();

    // we need to set enable for this to work, so do this last
    tmc6200_write_conf();
}

void tmc6200_write_conf(void) {
    // Driver comms OK, configure TMC6200
    tmc6200_writeInt(0, TMC6200_GCONF,
                     (0UL << TMC6200_DISABLE_SHIFT) |
                     (0UL << TMC6200_SINGLELINE_SHIFT) |
                     (1UL << TMC6200_FAULTDIRECT_SHIFT));
    tmc6200_writeInt(0, TMC6200_GSTAT, 0xFFFF);
    tmc6200_writeInt(0, TMC6200_SHORT_CONF, (1UL << TMC6200_DISABLE_S2G_SHIFT) | (1UL << TMC6200_DISABLE_S2VS_SHIFT));
}

#endif
