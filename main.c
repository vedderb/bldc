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
#include "comm_can.h"
#include "ws2811.h"
#include "led_external.h"
#include "encoder.h"
#include "servo.h"
#include "servo_simple.h"
#include "utils.h"
#include "nrf_driver.h"
#include "rfhelp.h"
#include "spi_sw.h"

/*
 * Timers used:
 * TIM7: servo
 * TIM1: mcpwm
 * TIM2: mcpwm
 * TIM12: mcpwm
 * TIM8: mcpwm
 * TIM3: servo_dec/Encoder (HW_R2)/servo_simple
 * TIM4: WS2811/WS2812 LEDs/Encoder (other HW)
 *
 * DMA/stream	Device		Function
 * 1, 2			I2C1		Nunchuk, temp on rev 4.5
 * 1, 7			I2C1		Nunchuk, temp on rev 4.5
 * 1, 1			UART3		HW_R2
 * 1, 3			UART3		HW_R2
 * 2, 2			UART6		Other HW
 * 2, 7			UART6		Other HW
 * 2, 4			ADC			mcpwm
 * 1, 0			TIM4		WS2811/WS2812 LEDs CH1 (Ch 1)
 * 1, 3			TIM4		WS2811/WS2812 LEDs CH2 (Ch 2)
 *
 */

// Private variables


static THD_WORKING_AREA(periodic_thread_wa, 1024);
static THD_WORKING_AREA(timer_thread_wa, 128);

static THD_FUNCTION(periodic_thread, arg) {
	(void)arg;

	chRegSetThreadName("Main periodic");

	for(;;) {
		if (mc_interface_get_state() == MC_STATE_RUNNING) {
			ledpwm_set_intensity(LED_GREEN, 1.0);
		} else {
			ledpwm_set_intensity(LED_GREEN, 0.2);
		}

		mc_fault_code fault = mc_interface_get_fault();
		if (fault != FAULT_CODE_NONE) {
			for (int i = 0;i < (int)fault;i++) {
				ledpwm_set_intensity(LED_RED, 1.0);
				chThdSleepMilliseconds(250);
				ledpwm_set_intensity(LED_RED, 0.0);
				chThdSleepMilliseconds(250);
			}

			chThdSleepMilliseconds(500);
		} else {
			ledpwm_set_intensity(LED_RED, 0.0);
		}

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

			default:
				break;
		}
		}

		chThdSleepMilliseconds(10);

//		chThdSleepMilliseconds(40);
//		volatile const mc_configuration *conf = mc_interface_get_configuration();
//		float vq = mcpwm_foc_get_vq();
//		float iq = mc_interface_get_tot_current_directional();
//		float linkage = conf->foc_motor_flux_linkage;
//		float speed = ((2.0 * M_PI) / 60.0) * mc_interface_get_rpm();
//
//		if (iq < -6.0) {
//			float res = vq / (linkage * speed * iq);
//			res *= 2.0 / 3.0;
//			static float res_filtered = 0.0;
//			UTILS_LP_FAST(res_filtered, res, 0.02);
//			commands_printf("Res: %.4f", (double)res_filtered);
//		}

//		chThdSleepMilliseconds(40);
//		commands_printf("Max: %.2f Min: %.2f",
//				(double)mc_interface_get_configuration()->lo_current_motor_max_now,
//				(double)mc_interface_get_configuration()->lo_current_motor_min_now);
	}
}

static THD_FUNCTION(timer_thread, arg) {
	(void)arg;

	chRegSetThreadName("msec_timer");

	for(;;) {
		packet_timerfunc();
		chThdSleepMilliseconds(1);
	}
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

	chThdSleepMilliseconds(1000);

	hw_init_gpio();
	LED_RED_OFF();
	LED_GREEN_OFF();

	conf_general_init();
	ledpwm_init();

	mc_configuration mcconf;
	conf_general_read_mc_configuration(&mcconf);
	mc_interface_init(&mcconf);

	commands_init();
	comm_usb_init();

#if CAN_ENABLE
	comm_can_init();
#endif

	app_configuration appconf;
	conf_general_read_app_configuration(&appconf);
	app_set_configuration(&appconf);

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
	}
#endif

	timeout_init();
	timeout_configure(appconf.timeout_msec, appconf.timeout_brake_current);

#if WS2811_ENABLE
	ws2811_init();
#if !WS2811_TEST
	led_external_init();
#endif
#endif

#if SERVO_OUT_ENABLE
#if SERVO_OUT_SIMPLE
	servo_simple_init();
#else
	servo_init();
#endif
#endif

	// Threads
	chThdCreateStatic(periodic_thread_wa, sizeof(periodic_thread_wa), NORMALPRIO, periodic_thread, NULL);
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);

#if WS2811_TEST
	unsigned int color_ind = 0;
	const int num = 4;
	const uint32_t colors[] = {COLOR_RED, COLOR_GOLD, COLOR_GRAY, COLOR_MAGENTA, COLOR_BLUE};
	const int brightness_set = 100;

	for (;;) {
		chThdSleepMilliseconds(1000);

		for (int i = 0;i < brightness_set;i++) {
			ws2811_set_brightness(i);
			chThdSleepMilliseconds(10);
		}

		chThdSleepMilliseconds(1000);

		for(int i = -num;i <= WS2811_LED_NUM;i++) {
			ws2811_set_led_color(i - 1, COLOR_BLACK);
			ws2811_set_led_color(i + num, colors[color_ind]);

			ws2811_set_led_color(0, COLOR_RED);
			ws2811_set_led_color(WS2811_LED_NUM - 1, COLOR_GREEN);

			chThdSleepMilliseconds(50);
		}

		for (int i = 0;i < brightness_set;i++) {
			ws2811_set_brightness(brightness_set - i);
			chThdSleepMilliseconds(10);
		}

		color_ind++;
		if (color_ind >= sizeof(colors) / sizeof(uint32_t)) {
			color_ind = 0;
		}

		static int asd = 0;
		asd++;
		if (asd >= 3) {
			asd = 0;

			for (unsigned int i = 0;i < sizeof(colors) / sizeof(uint32_t);i++) {
				ws2811_set_all(colors[i]);

				for (int i = 0;i < brightness_set;i++) {
					ws2811_set_brightness(i);
					chThdSleepMilliseconds(2);
				}

				chThdSleepMilliseconds(100);

				for (int i = 0;i < brightness_set;i++) {
					ws2811_set_brightness(brightness_set - i);
					chThdSleepMilliseconds(2);
				}
			}
		}
	}
#endif

	for(;;) {
		chThdSleepMilliseconds(10);

		if (encoder_is_configured()) {
			//		comm_can_set_pos(0, encoder_read_deg());
		}
	}
}
