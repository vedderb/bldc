/*
	Copyright 2020 Benjamin Vedder	benjamin@vedder.se

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

#include "si8900.h"
#include "conf_general.h"
#include "terminal.h"
#include "commands.h"

#ifdef HW_HAS_SI8900

// Private variables
static THD_FUNCTION(si_read_thread, arg);
static THD_WORKING_AREA(si_read_thread_wa, 512);
static volatile float m_voltages[3];

// Private functions
static void terminal_read(int argc, const char **argv);

static SerialConfig uart_cfg = {
		115200,
		0,
		USART_CR2_LINEN,
		0
};

void si8900_init(void) {
	sdStart(&HW_SI8900_DEV, &uart_cfg);
	palSetPadMode(HW_SI8900_TX_PORT, HW_SI8900_TX_PIN, PAL_MODE_ALTERNATE(HW_SI8900_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
	palSetPadMode(HW_SI8900_RX_PORT, HW_SI8900_RX_PIN, PAL_MODE_ALTERNATE(HW_SI8900_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
	chThdCreateStatic(si_read_thread_wa, sizeof(si_read_thread_wa), NORMALPRIO, si_read_thread, NULL);

	terminal_register_command_callback(
			"si8900_read",
			"Read and print ADC values for 10 seconds.",
			0,
			terminal_read);
}

float si8900_get_voltage(int channel) {
	float res = -1.0;

	if (channel >= 0 && channel < 3) {
		res = m_voltages[channel];
	}

	return res;
}

float si8900_get_val_rel(int channel) {
	return si8900_get_voltage(channel) / 3.3;
}

static void terminal_read(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	for (int i = 0;i < 100;i++) {
		commands_printf("[IN0 IN1 IN2] = [%.3f %.3f %.3f]",
				(double)si8900_get_voltage(0),
				(double)si8900_get_voltage(1),
				(double)si8900_get_voltage(2));
		chThdSleepMilliseconds(100);
	}

	commands_printf("Done\n");
}

static THD_FUNCTION(si_read_thread, arg) {
	(void)arg;
	chRegSetThreadName("SI8900");

	while(!chThdShouldTerminateX()) {
		uint8_t txb[1];
		size_t tx_len = 1;
		uint8_t rxb[3];
		size_t rx_len = 3;

		// Baud rate adjustment
		txb[0] = 0xAA;
		tx_len = 1;
		for (int i = 0;i < 4;i++) {
			sdWriteTimeout(&HW_SI8900_DEV, txb, tx_len, MS2ST(10));
			rx_len = 1;
			size_t res = sdReadTimeout(&HW_SI8900_DEV, rxb, rx_len, 5);
			if (res == rx_len && rxb[0] == 0x55) {
				break;
			}
		}

		for (int i = 0;i < 3;i++) {
			if (i == 0) {
				txb[0] = SI8900_CNFG_0 | SI8900_CNFG_0_PGA | SI8900_CNFG_0_MODE;
			} else if (i == 1) {
				txb[0] = SI8900_CNFG_0 | SI8900_CNFG_0_PGA | SI8900_CNFG_0_MODE | SI8900_CNFG_0_MX0;
			} else {
				txb[0] = SI8900_CNFG_0 | SI8900_CNFG_0_PGA | SI8900_CNFG_0_MODE | SI8900_CNFG_0_MX1;
			}

			tx_len = 1;

			for (int j = 0;j < 4;j++) {
				sdWriteTimeout(&HW_SI8900_DEV, txb, tx_len, MS2ST(10));
				rx_len = 1;
				size_t res = sdReadTimeout(&HW_SI8900_DEV, rxb, rx_len, 5);
				if (res == rx_len && rxb[0] == txb[0]) {
					break;
				}
			}

			rx_len = 2;
			size_t res = sdReadTimeout(&HW_SI8900_DEV, rxb, rx_len, MS2ST(10));
			if (res == rx_len) {
				m_voltages[i] = (float)((((uint16_t)rxb[0] & 0b00001111) << 6) |
						(((uint16_t)rxb[1] >> 1) & 0b00111111)) / 1023.0 * 3.3;
			}
		}

		chThdSleepMilliseconds(20);
		while(sdGetTimeout(&HW_SI8900_DEV, TIME_IMMEDIATE) != MSG_TIMEOUT){
			chThdSleepMilliseconds(1);
		};
	}
}

#endif
