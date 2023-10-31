/*
	Copyright 2016 - 2021 Benjamin Vedder	benjamin@vedder.se

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

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "mc_interface.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "ledpwm.h"
#include "comm_usb.h"
#include "ledpwm.h"
#include "terminal.h"
#include "hw.h"
#include "app.h"
#include "packet.h"
#include "commands.h"
#include "timeout.h"
#include "encoder/encoder.h"
#include "servo_simple.h"
#include "utils_math.h"
#include "nrf_driver.h"
#include "rfhelp.h"
#include "spi_sw.h"
#include "timer.h"
#include "imu.h"
#include "flash_helper.h"
#if HAS_BLACKMAGIC
#include "bm_if.h"
#endif
#include "shutdown.h"
#include "mempools.h"
#include "events.h"
#include "main.h"
#ifdef CAN_ENABLE
#include "comm_can.h"

#define CAN_FRAME_MAX_PL_SIZE	8
#endif

#ifdef USE_LISPBM
#include "lispif.h"
#endif

/*
 * HW resources used:
 *
 * TIM1: mcpwm
 * TIM2: mcpwm_foc
 * TIM5: timer
 * TIM8: mcpwm
 * TIM3: servo_dec/Encoder (HW_R2)/servo_simple
 * TIM4: WS2811/WS2812 LEDs/Encoder (other HW)
 *
 * DMA/stream	Device		Function
 * 1, 2			I2C1		Nunchuk, temp on rev 4.5
 * 1, 7			I2C1		Nunchuk, temp on rev 4.5
 * 2, 4			ADC			mcpwm
 *
 */

// Private variables
static THD_WORKING_AREA(periodic_thread_wa, 256);
static THD_WORKING_AREA(led_thread_wa, 256);
static THD_WORKING_AREA(flash_integrity_check_thread_wa, 256);
static volatile bool m_init_done = false;

static THD_FUNCTION(flash_integrity_check_thread, arg) {
	(void)arg;

	chRegSetThreadName("Flash check");
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_CRC, ENABLE);

	for(;;) {
		if (flash_helper_verify_flash_memory_chunk() == FAULT_CODE_FLASH_CORRUPTION) {
			NVIC_SystemReset();
		}

		chThdSleepMilliseconds(6);
	}
}

static THD_FUNCTION(led_thread, arg) {
	(void)arg;

	chRegSetThreadName("Main LED");

	for(;;) {
		mc_state state1 = mc_interface_get_state();
		mc_interface_select_motor_thread(2);
		mc_state state2 = mc_interface_get_state();
		mc_interface_select_motor_thread(1);
		if ((state1 == MC_STATE_RUNNING) || (state2 == MC_STATE_RUNNING)) {
			ledpwm_set_intensity(LED_GREEN, 1.0);
		} else {
			ledpwm_set_intensity(LED_GREEN, 0.2);
		}

		mc_fault_code fault = mc_interface_get_fault();
		mc_interface_select_motor_thread(2);
		mc_fault_code fault2 = mc_interface_get_fault();
		mc_interface_select_motor_thread(1);
		if (fault != FAULT_CODE_NONE || fault2 != FAULT_CODE_NONE) {
			for (int i = 0;i < (int)fault;i++) {
				ledpwm_set_intensity(LED_RED, 1.0);
				chThdSleepMilliseconds(250);
				ledpwm_set_intensity(LED_RED, 0.0);
				chThdSleepMilliseconds(250);
			}

			chThdSleepMilliseconds(500);

			for (int i = 0;i < (int)fault2;i++) {
				ledpwm_set_intensity(LED_RED, 1.0);
				chThdSleepMilliseconds(250);
				ledpwm_set_intensity(LED_RED, 0.0);
				chThdSleepMilliseconds(250);
			}

			chThdSleepMilliseconds(500);
		} else {
			ledpwm_set_intensity(LED_RED, 0.0);
		}

		chThdSleepMilliseconds(10);
	}
}

static THD_FUNCTION(periodic_thread, arg) {
	(void)arg;

	chRegSetThreadName("Main periodic");

	for(;;) {
		if (mc_interface_get_state() == MC_STATE_DETECTING) {
			commands_send_rotor_pos(mcpwm_get_detect_pos());
		}

		disp_pos_mode display_mode = commands_get_disp_pos_mode();

		switch (display_mode) {
		case DISP_POS_MODE_ENCODER:
			commands_send_rotor_pos(encoder_read_deg());
			break;

		case DISP_POS_MODE_PID_POS:
			commands_send_rotor_pos(mc_interface_get_pid_pos_now());
			break;

		case DISP_POS_MODE_PID_POS_ERROR:
			commands_send_rotor_pos(utils_angle_difference(mc_interface_get_pid_pos_set(), mc_interface_get_pid_pos_now()));
			break;

		default:
			break;
		}

		if (mc_interface_get_configuration()->motor_type == MOTOR_TYPE_FOC) {
			switch (display_mode) {
			case DISP_POS_MODE_OBSERVER:
				commands_send_rotor_pos(mcpwm_foc_get_phase_observer());
				break;

			case DISP_POS_MODE_ENCODER_OBSERVER_ERROR:
				commands_send_rotor_pos(utils_angle_difference(mcpwm_foc_get_phase_observer(), mcpwm_foc_get_phase_encoder()));
				break;

			case DISP_POS_MODE_HALL_OBSERVER_ERROR:
				commands_send_rotor_pos(utils_angle_difference(mcpwm_foc_get_phase_observer(), mcpwm_foc_get_phase_hall()));
				break;

			default:
				break;
			}
		}
	 
		HW_TRIM_HSI(); // Compensate HSI for temperature

		chThdSleepMilliseconds(10);
	}
}

// When assertions enabled halve PWM frequency. The control loop ISR runs 40% slower
void assert_failed(uint8_t* file, uint32_t line) {
	commands_printf("Wrong parameters value: file %s on line %d\r\n", file, line);
	mc_interface_release_motor();
	while(1) {
		chThdSleepMilliseconds(1);
	}
}

bool main_init_done(void) {
	return m_init_done;
}

int main(void) {
	halInit();
	chSysInit();

	// Initialize the enable pins here and disable them
	// to avoid excessive current draw at boot because of
	// floating pins.
#ifdef HW_HAS_DRV8313
	INIT_BR();
#endif

	HW_EARLY_INIT();

#ifdef BOOT_OK_GPIO
	palSetPadMode(BOOT_OK_GPIO, BOOT_OK_PIN, PAL_MODE_OUTPUT_PUSHPULL);
	palClearPad(BOOT_OK_GPIO, BOOT_OK_PIN);
#endif

	chThdSleepMilliseconds(100);

	mempools_init();
	events_init();
	hw_init_gpio();
	LED_RED_OFF();
	LED_GREEN_OFF();

	timer_init();
	conf_general_init();

	if (flash_helper_verify_flash_memory() == FAULT_CODE_FLASH_CORRUPTION)	{
		// Loop here, it is not safe to run any code
		while (1) {
			chThdSleepMilliseconds(100);
			LED_RED_ON();
			chThdSleepMilliseconds(75);
			LED_RED_OFF();
		}
	}

	ledpwm_init();
	mc_interface_init();

	commands_init();

#if COMM_USE_USB
	comm_usb_init();
#endif

	app_uartcomm_initialize();
	app_configuration *appconf = mempools_alloc_appconf();
	conf_general_read_app_configuration(appconf);
	app_uartcomm_start(UART_PORT_BUILTIN);
	app_uartcomm_start(UART_PORT_EXTRA_HEADER);
	app_set_configuration(appconf);

	// This reads the appconf, that must be initialized first.
#if CAN_ENABLE
	comm_can_init();
#endif

#ifdef HW_HAS_PERMANENT_NRF
	conf_general_permanent_nrf_found = nrf_driver_init();
	if (conf_general_permanent_nrf_found) {
		rfhelp_restart();
	} else {
		nrf_driver_stop();
		// Set the nrf SPI pins to the general SPI interface so that
		// an external NRF can be used with the NRF app.
		spi_sw_change_pins(
				HW_SPI_PORT_NSS, HW_SPI_PIN_NSS,
				HW_SPI_PORT_SCK, HW_SPI_PIN_SCK,
				HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI,
				HW_SPI_PORT_MISO, HW_SPI_PIN_MISO);
		HW_PERMANENT_NRF_FAILED_HOOK();
	}
#endif

	// Threads
	chThdCreateStatic(led_thread_wa, sizeof(led_thread_wa), NORMALPRIO, led_thread, NULL);
	chThdCreateStatic(periodic_thread_wa, sizeof(periodic_thread_wa), NORMALPRIO, periodic_thread, NULL);
	chThdCreateStatic(flash_integrity_check_thread_wa, sizeof(flash_integrity_check_thread_wa), LOWPRIO, flash_integrity_check_thread, NULL);

	timeout_init();
	timeout_configure(appconf->timeout_msec, appconf->timeout_brake_current, appconf->kill_sw_mode);

#if HAS_BLACKMAGIC
	bm_init();
#endif

	shutdown_init();

	imu_reset_orientation();

	chThdSleepMilliseconds(500);
	m_init_done = true;

#ifdef BOOT_OK_GPIO
	palSetPad(BOOT_OK_GPIO, BOOT_OK_PIN);
#endif

#ifdef CAN_ENABLE
	// Transmit a CAN boot-frame to notify other nodes on the bus about it.
	if (appconf->can_mode == CAN_MODE_VESC) {
		comm_can_transmit_eid(
				app_get_configuration()->controller_id | (CAN_PACKET_NOTIFY_BOOT << 8),
				(uint8_t *)HW_NAME, (strlen(HW_NAME) <= CAN_FRAME_MAX_PL_SIZE) ?
						strlen(HW_NAME) : CAN_FRAME_MAX_PL_SIZE);
	}
#endif

	mempools_free_appconf(appconf);

	for(;;) {
		chThdSleepMilliseconds(10);
	}
}
