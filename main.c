/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
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

	chThdSleepMilliseconds(1000);

	hw_init_gpio();
	LED_RED_OFF();
	LED_GREEN_OFF();

	conf_general_init();
	ledpwm_init();

	mc_configuration mcconf;
	conf_general_read_mc_configuration(&mcconf);

#if ENCODER_ENABLE
	encoder_init(mcconf.m_encoder_counts);
#endif

	mc_interface_init(&mcconf);

	commands_init();
	comm_usb_init();

	app_configuration appconf;
	conf_general_read_app_configuration(&appconf);
	app_init(&appconf);

	timeout_init();
	timeout_configure(appconf.timeout_msec, appconf.timeout_brake_current);

#if CAN_ENABLE
	comm_can_init();
#endif

#if WS2811_ENABLE
	ws2811_init();
	led_external_init();
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

	for(;;) {
		chThdSleepMilliseconds(10);

#if ENCODER_ENABLE
//		comm_can_set_pos(0, encoder_read_deg());
#endif
	}
}
