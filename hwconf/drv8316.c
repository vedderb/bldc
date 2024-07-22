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
#ifdef HW_HAS_DRV8316

#include "drv8316.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils_math.h"
#include "terminal.h"
#include "commands.h"
#include <string.h>
#include <stdio.h>
#include "mc_interface.h"

// Private functions
static uint16_t spi_exchange(uint16_t x);
static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length);
static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);
static void terminal_read_reg(int argc, const char **argv);
static void terminal_write_reg(int argc, const char **argv);
// static void terminal_set_oc_adj(int argc, const char **argv);
static void terminal_print_faults(int argc, const char **argv);
static void terminal_reset_faults(int argc, const char **argv);

// Private variables
static char m_fault_print_buffer[120];
static mutex_t m_spi_mutex;

void drv8316_init(void)
{
	chMtxObjectInit(&m_spi_mutex);

	// Initialise DRV8316 SPI
	palSetPadMode(DRV8316_MISO_GPIO, DRV8316_MISO_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(DRV8316_SCK_GPIO, DRV8316_SCK_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(DRV8316_CS_GPIO, DRV8316_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(DRV8316_MOSI_GPIO, DRV8316_MOSI_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	palSetPad(DRV8316_CS_GPIO, DRV8316_CS_PIN); // Chip select rests high

	chThdSleepMilliseconds(100);

	// Unlock all registers for write access
	uint8_t UNLOCK_ALL_REGISTERS = 0b011;
	uint16_t ctrl_reg_1_value = drv8316_read_reg(DRV8316_CTRL_REG_1);
	ctrl_reg_1_value &= 0xF8;				  // Keeps reserved bits (3-7) the same
	ctrl_reg_1_value |= UNLOCK_ALL_REGISTERS; // Command all registers to unlock
	drv8316_write_reg(DRV8316_CTRL_REG_1, ctrl_reg_1_value);
	chThdSleepMilliseconds(10);

	terminal_register_command_callback(
		"drv8316_read_reg",
		"Read a register from the DRV8316 and print it.",
		"[reg]",
		terminal_read_reg);

	terminal_register_command_callback(
		"drv8316_write_reg",
		"Write to a DRV8316 register.",
		"[reg] [hexvalue]",
		terminal_write_reg);

	terminal_register_command_callback(
		"drv8316_print_faults",
		"Print all current DRV8316 faults.",
		0,
		terminal_print_faults);

	terminal_register_command_callback(
		"drv8316_reset_faults",
		"Resets all DRV8316 faults.",
		0,
		terminal_reset_faults);
}

// /**
//  * Read the fault codes of the DRV8316.
//  */
uint32_t drv8316_read_faults(void)
{
	uint8_t ic_status = drv8316_read_reg(DRV8316_IC_STATUS_REG);
	uint16_t status_reg1 = drv8316_read_reg(DRV8316_STATUS_REG_1);
	uint32_t status_reg2 = drv8316_read_reg(DRV8316_STATUS_REG_2) & 0x7F; // Bit 7 is reserved

	return ic_status | (status_reg1 << 8) | (status_reg2 << 16);
}

// /**
//  * Reset all latched faults.
//  */
void drv8316_reset_faults(void)
{
	int reg = drv8316_read_reg(DRV8316_CTRL_REG_2);

	uint8_t CLR_FLT = 0b1;
	reg |= CLR_FLT;

	drv8316_write_reg(DRV8316_CTRL_REG_2, reg);
}

char *drv8316_faults_to_string(unsigned long faults)
{
	commands_printf("faults: %d", faults);

	if (faults == 0)
	{
		strcpy(m_fault_print_buffer, "No DRV8316 faults");
	}
	else
	{
		uint8_t ic_status_faults = faults & 0xFF;
		uint8_t status_reg1_faults = (faults >> 8) & 0xFF;
		uint8_t status_reg2_faults = (faults >> 16) & 0xFF;

		commands_printf("ic_status_faults: %x", ic_status_faults);
		commands_printf("status_reg1_faults: %x", status_reg1_faults);
		commands_printf("status_reg2_faults: %x", status_reg2_faults);

		strcpy(m_fault_print_buffer, "|");

		// IC Status Register
		if (ic_status_faults & DRV8316_IC_FAULT_BUCK_FLT)
		{
			strcat(m_fault_print_buffer, " DRV8316_IC_FAULT_BUCK_FLT |");
		}
		if (ic_status_faults & DRV8316_IC_FAULT_SPI_FLT)
		{
			strcat(m_fault_print_buffer, " DRV8316_IC_FAULT_SPI_FLT |");
		}
		if (ic_status_faults & DRV8316_IC_FAULT_OCP)
		{
			strcat(m_fault_print_buffer, " DRV8316_IC_FAULT_OCP |");
		}
		if (ic_status_faults & DRV8316_IC_FAULT_NPOR)
		{
			strcat(m_fault_print_buffer, " DRV8316_IC_FAULT_NPOR |");
		}
		if (ic_status_faults & DRV8316_IC_FAULT_OVP)
		{
			strcat(m_fault_print_buffer, " DRV8316_IC_FAULT_OVP |");
		}
		if (ic_status_faults & DRV8316_IC_FAULT_OT)
		{
			strcat(m_fault_print_buffer, " DRV8316_IC_FAULT_OT |");
		}
		if (ic_status_faults & DRV8316_IC_FAULT_DEVICE_FLT)
		{
			strcat(m_fault_print_buffer, " DRV8316_IC_FAULT_DEVICE_FLT |");
		}

		// Status Register 1 Faults
		if (status_reg1_faults & DRV8316_FAULT_STS_1_OTW)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OTW |");
		}

		if (status_reg1_faults & DRV8316_FAULT_STS_1_OTS)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OTS |");
		}

		if (status_reg1_faults & DRV8316_FAULT_STS_1_OCP_HC)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OCP_HC |");
		}

		if (status_reg1_faults & DRV8316_FAULT_STS_1_OCP_LC)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OCP_LC |");
		}

		if (status_reg1_faults & DRV8316_FAULT_STS_1_OCP_HB)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OCP_HB |");
		}

		if (status_reg1_faults & DRV8316_FAULT_STS_1_OCP_LB)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OCP_LB |");
		}

		if (status_reg1_faults & DRV8316_FAULT_STS_1_OCP_HA)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OCP_HA |");
		}

		if (status_reg1_faults & DRV8316_FAULT_STS_1_OCP_LA)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_1_OCP_LA |");
		}

		// Status Register 2 Faults
		if (status_reg2_faults & DRV8316_FAULT_STS_2_OTP_ERR)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_2_OTP_ERR |");
		}

		if (status_reg2_faults & DRV8316_FAULT_STS_2_BUCK_OCP)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_2_BUCK_OCP |");
		}

		if (status_reg2_faults & DRV8316_FAULT_STS_2_BUCK_UV)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_2_BUCK_UV |");
		}

		if (status_reg2_faults & DRV8316_FAULT_STS_2_VCP_UV)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_2_VCP_UV |");
		}

		if (status_reg2_faults & DRV8316_FAULT_STS_2_SPI_PARITY)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_2_SPI_PARITY |");
		}

		if (status_reg2_faults & DRV8316_FAULT_STS_2_SPI_SCLK_FLT)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_2_SPI_SCLK_FLT |");
		}

		if (status_reg2_faults & DRV8316_FAULT_STS_2_SPI_ADDR_FLT)
		{
			strcat(m_fault_print_buffer, " DRV8316_FAULT_STS_2_SPI_ADDR_FLT |");
		}
	}

	return m_fault_print_buffer;
}

unsigned int drv8316_read_reg(int reg)
{
	uint16_t out = 0;
	out |= (1 << 15);
	out |= (reg & 0x3F) << 9;
	uint16_t parity = out;
	parity ^= parity >> 8;
	parity ^= parity >> 4;
	parity ^= parity >> 2;
	parity ^= parity >> 1;
	out |= (parity & 1) << 8;

	chMtxLock(&m_spi_mutex);

	if (reg != 0)
	{
		spi_begin();
		spi_exchange(out);
		spi_end();
	}

	spi_begin();
	uint16_t res = spi_exchange(out);
	spi_end();

	chMtxUnlock(&m_spi_mutex);

	return res;
}

void drv8316_write_reg(uint8_t reg, uint8_t data)
{
	uint16_t out = 0;
	out |= (data & 0xFF);
	out |= (reg & 0x3F) << 9;

	uint16_t parity = out;
	parity ^= parity >> 8;
	parity ^= parity >> 4;
	parity ^= parity >> 2;
	parity ^= parity >> 1;
	out |= (parity & 1) << 8;

	chMtxLock(&m_spi_mutex);
	spi_begin();
	spi_exchange(out);
	spi_end();
	chMtxUnlock(&m_spi_mutex);
}

// Software SPI
static uint16_t spi_exchange(uint16_t x)
{
	uint16_t rx;
	spi_transfer(&rx, &x, 1);
	return rx;
}

static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length)
{
	for (int i = 0; i < length; i++)
	{
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
		uint16_t receive = 0;

		for (int bit = 0; bit < 16; bit++)
		{
			palWritePad(DRV8316_MOSI_GPIO, DRV8316_MOSI_PIN, send >> 15);
			send <<= 1;

			palSetPad(DRV8316_SCK_GPIO, DRV8316_SCK_PIN);
			spi_delay();

			palClearPad(DRV8316_SCK_GPIO, DRV8316_SCK_PIN);

			int samples = 0;
			samples += palReadPad(DRV8316_MISO_GPIO, DRV8316_MISO_PIN);
			__NOP();
			samples += palReadPad(DRV8316_MISO_GPIO, DRV8316_MISO_PIN);
			__NOP();
			samples += palReadPad(DRV8316_MISO_GPIO, DRV8316_MISO_PIN);
			__NOP();
			samples += palReadPad(DRV8316_MISO_GPIO, DRV8316_MISO_PIN);
			__NOP();
			samples += palReadPad(DRV8316_MISO_GPIO, DRV8316_MISO_PIN);

			receive <<= 1;
			if (samples > 2)
			{
				receive |= 1;
			}

			spi_delay();
		}

		if (in_buf)
		{
			in_buf[i] = receive;
		}
	}
}

static void spi_begin(void)
{
	spi_delay();
#ifdef DRV8316_CS_GPIO2
	if (mc_interface_motor_now() == 2)
	{
		palClearPad(DRV8316_CS_GPIO2, DRV8316_CS_PIN2);
	}
	else
	{
		palClearPad(DRV8316_CS_GPIO, DRV8316_CS_PIN);
	}
#else
	palClearPad(DRV8316_CS_GPIO, DRV8316_CS_PIN);
#endif
	spi_delay();
}

static void spi_end(void)
{
	spi_delay();

#ifdef DRV8316_CS_GPIO2
	if (mc_interface_motor_now() == 2)
	{
		palSetPad(DRV8316_CS_GPIO2, DRV8316_CS_PIN2);
	}
	else
	{
		palSetPad(DRV8316_CS_GPIO, DRV8316_CS_PIN);
	}
#else
	palSetPad(DRV8316_CS_GPIO, DRV8316_CS_PIN);
#endif
	spi_delay();
}

static void spi_delay(void)
{
	for (volatile int i = 0; i < 40; i++)
	{
		__NOP();
	}
}

static void terminal_read_reg(int argc, const char **argv)
{
	if (argc == 2)
	{
		int reg = -1;
		sscanf(argv[1], "%d", &reg);

		if (reg >= 0)
		{
			unsigned int res = drv8316_read_reg(reg);
			char bl[9];
			char bh[9];

			utils_byte_to_binary((res >> 8) & 0xFF, bh);
			utils_byte_to_binary(res & 0xFF, bl);

			commands_printf("Reg 0x%02x: %s %s (0x%04x)\n", reg, bh, bl, res);
		}
		else
		{
			commands_printf("Invalid argument(s).\n");
		}
	}
	else
	{
		commands_printf("This command requires one argument.\n");
	}
}

static void terminal_write_reg(int argc, const char **argv)
{
	if (argc == 3)
	{
		int reg = -1;
		int val = -1;
		sscanf(argv[1], "%d", &reg);
		sscanf(argv[2], "%x", &val);

		if (reg >= 0 && val >= 0)
		{
			drv8316_write_reg(reg, val);
			unsigned int res = drv8316_read_reg(reg);
			char bl[9];
			char bh[9];

			utils_byte_to_binary((res >> 8) & 0xFF, bh);
			utils_byte_to_binary(res & 0xFF, bl);

			commands_printf("New reg value 0x%02x: %s %s (0x%04x)\n", reg, bh, bl, res);
		}
		else
		{
			commands_printf("Invalid argument(s).\n");
		}
	}
	else
	{
		commands_printf("This command requires two arguments.\n");
	}
}

// static void terminal_set_oc_adj(int argc, const char **argv) {
// 	if (argc == 2) {
// 		int val = -1;
// 		sscanf(argv[1], "%d", &val);

// 		if (val >= 0 && val < 32) {
// 			drv8316_set_oc_adj(val);
// 			unsigned int res = drv8316_read_reg(5);
// 			char bl[9];
// 			char bh[9];

// 			utils_byte_to_binary((res >> 8) & 0xFF, bh);
// 			utils_byte_to_binary(res & 0xFF, bl);

// 			commands_printf("New reg value 0x%02x: %s %s (0x%04x)\n", 2, bh, bl, res);
// 		} else {
// 			commands_printf("Invalid argument(s).\n");
// 		}
// 	} else {
// 		commands_printf("This command requires one argument.\n");
// 	}
// }

static void terminal_print_faults(int argc, const char **argv)
{
	(void)argc;
	(void)argv;
	commands_printf(drv8316_faults_to_string(drv8316_read_faults()));
}

static void terminal_reset_faults(int argc, const char **argv)
{
	(void)argc;
	(void)argv;

	drv8316_reset_faults();
	commands_printf("Reset all faults");
}

#endif
