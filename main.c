/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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

#include "main.h"
#include "mcpwm.h"
#include "ledpwm.h"
#include "comm.h"
#include "ledpwm.h"
#include "terminal.h"
#include "hw.h"
#include "app.h"
#include "packet.h"

/*
 * Timers used:
 * TIM7: servo
 * TIM1: mcpwm
 * TIM2: mcpwm
 * TIM4: mcpwm
 * TIM8: mcpwm
 * TIM3: servo_dec
 */

/*
 * Notes:
 *
 * Disable USB VBUS sensing:
 * ChibiOS-RT-master/os/hal/platforms/STM32/OTGv1/usb_lld.c
 *
 * change
 * otgp->GCCFG = GCCFG_VBUSASEN | GCCFG_VBUSBSEN | GCCFG_PWRDWN;
 * to
 * otgp->GCCFG = GCCFG_NOVBUSSENS | GCCFG_PWRDWN;
 *
 * This should be handled automatically with the latest version of
 * ChibiOS since I have added an option to the makefile.
 *
 */

// Private variables
#define ADC_SAMPLE_MAX_LEN		4000
static volatile int16_t curr0_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t curr1_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t ph1_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t ph2_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t ph3_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t vzero_samples[ADC_SAMPLE_MAX_LEN];
static volatile uint8_t status_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t curr_fir_samples[ADC_SAMPLE_MAX_LEN];

static volatile int sample_len = 1000;
static volatile int sample_int = 1;
static volatile int sample_ready = 1;
static volatile int sample_now = 0;
static volatile int sample_at_start = 0;
static volatile int was_start_sample = 0;
static volatile int start_comm = 0;
static volatile float main_last_adc_duration = 0.0;

static WORKING_AREA(periodic_thread_wa, 1024);
static WORKING_AREA(sample_send_thread_wa, 1024);
static WORKING_AREA(timer_thread_wa, 128);

static Thread *sample_send_tp;

static msg_t periodic_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Main periodic");

	int fault_print = 0;

	for(;;) {
		if (mcpwm_get_state() == MC_STATE_RUNNING) {
			ledpwm_set_intensity(LED_GREEN, 1.0);
		} else {
			ledpwm_set_intensity(LED_GREEN, 0.2);
		}

		if (mcpwm_get_fault() != FAULT_CODE_NONE) {
			ledpwm_set_intensity(LED_RED, 1.0);
			if (!fault_print && AUTO_PRINT_FAULTS) {
				fault_print = 1;
				comm_print_fault_code(mcpwm_get_fault());
			}
		} else {
			ledpwm_set_intensity(LED_RED, 0.0);
			fault_print = 0;
		}

		if (mcpwm_get_state() == MC_STATE_DETECTING) {
			comm_send_rotor_pos(mcpwm_get_detect_pos());
		}

		chThdSleepMilliseconds(25);
	}

	return 0;
}

static msg_t sample_send_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Main sample");

	sample_send_tp = chThdSelf();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		for (int i = 0;i < sample_len;i++) {
			uint8_t buffer[20];
			int index = 0;

			buffer[index++] = curr0_samples[i] >> 8;
			buffer[index++] = curr0_samples[i];
			buffer[index++] = curr1_samples[i] >> 8;
			buffer[index++] = curr1_samples[i];
			buffer[index++] = ph1_samples[i] >> 8;
			buffer[index++] = ph1_samples[i];
			buffer[index++] = ph2_samples[i] >> 8;
			buffer[index++] = ph2_samples[i];
			buffer[index++] = ph3_samples[i] >> 8;
			buffer[index++] = ph3_samples[i];
			buffer[index++] = vzero_samples[i] >> 8;
			buffer[index++] = vzero_samples[i];
			buffer[index++] = status_samples[i];
			buffer[index++] = curr_fir_samples[i] >> 8;
			buffer[index++] = curr_fir_samples[i];

			comm_send_samples(buffer, index);

			// TODO: wait??
			chThdSleep(2);
		}
	}

	return 0;
}

static msg_t timer_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("msec_timer");

	for(;;) {
		packet_timerfunc();
		chThdSleepMilliseconds(1);
	}

	return 0;
}

/*
 * Called every time new ADC values are available. Note that
 * the ADC is initialized from mcpwm.c
 */
void main_dma_adc_handler(void) {
	ledpwm_update_pwm();

	if (sample_at_start && (mcpwm_get_state() == MC_STATE_RUNNING ||
			start_comm != mcpwm_get_comm_step())) {
		sample_now = 0;
		sample_ready = 0;
		was_start_sample = 1;
		sample_at_start = 0;
	}

	static int a = 0;
	if (!sample_ready) {
		a++;
		if (a >= sample_int) {
			a = 0;

			if (mcpwm_get_state() == MC_STATE_DETECTING) {
				curr0_samples[sample_now] = (int16_t)mcpwm_detect_currents[mcpwm_get_comm_step() - 1];
				curr1_samples[sample_now] = (int16_t)mcpwm_detect_currents_diff[mcpwm_get_comm_step() - 1];
			} else {
				curr0_samples[sample_now] = ADC_curr_norm_value[0];
				curr1_samples[sample_now] = ADC_curr_norm_value[1];
			}

			ph1_samples[sample_now] = ADC_V_L1 - mcpwm_vzero;
			ph2_samples[sample_now] = ADC_V_L2 - mcpwm_vzero;
			ph3_samples[sample_now] = ADC_V_L3 - mcpwm_vzero;
			vzero_samples[sample_now] = mcpwm_vzero;

			curr_fir_samples[sample_now] = (int16_t)(mcpwm_get_tot_current_filtered() * 100);

			uint8_t tmp;

			if (was_start_sample) {
				if (mcpwm_get_state() == MC_STATE_OFF) {
					tmp = 1;
				} else if (mcpwm_get_state() == MC_STATE_RUNNING) {
					tmp = 2;
				} else {
					tmp = 3;
				}
			} else {
				tmp = mcpwm_read_hall_phase();
			}

			status_samples[sample_now] = mcpwm_get_comm_step() | (tmp << 3);

			sample_now++;

			if (sample_now == sample_len) {
				sample_ready = 1;
				sample_now = 0;
				was_start_sample = 0;
				chSysLockFromIsr();
				chEvtSignalI(sample_send_tp, (eventmask_t) 1);
				chSysUnlockFromIsr();
			}

			main_last_adc_duration = mcpwm_get_last_adc_isr_duration();
		}
	}
}

float main_get_last_adc_isr_duration(void) {
	return main_last_adc_duration;
}

void main_process_packet(unsigned char *data, unsigned char len) {
	if (!len) {
		return;
	}

	int16_t value16;
	int32_t value32;

	switch (data[0]) {
	case 1:
		// Sample and print data
		value16 = (int)data[1] << 8 | (int)data[2];
		if (value16 > ADC_SAMPLE_MAX_LEN) {
			value16 = ADC_SAMPLE_MAX_LEN;
		}
		sample_len = value16;
		sample_int = data[3];
		sample_now = 0;
		sample_ready = 0;
		break;

	case 2:
		// Duty Control
		value16 = (int)data[1] << 8 | (int)data[2];
		mcpwm_set_duty((float)value16 / 1000.0);
		break;

	case 3:
		// PID Control
		value32 = (int)data[1] << 24 | (int)data[2] << 16 | (int)data[3] << 8 | (int)data[4];
		mcpwm_set_pid_speed((float)value32);
		break;

	case 4:
		// Detect
		mcpwm_set_detect();
		break;

	case 5:
		// Reset Fault
		// TODO
		break;

	case 6:
		// Sample and print data at start
		value16 = (int)data[1] << 8 | (int)data[2];
		if (value16 > ADC_SAMPLE_MAX_LEN) {
			value16 = ADC_SAMPLE_MAX_LEN;
		}

		sample_len = value16;
		sample_int = data[3];
		sample_at_start = 1;
		start_comm = mcpwm_get_comm_step();
		break;

	case 7:
		// Current Control
		value16 = (int)data[1] << 8 | (int)data[2];
		mcpwm_set_current((float)value16 / 100.0);
		break;

	default:
		break;
	}
}

int main(void) {
	halInit();
	chSysInit();
	hw_init_gpio();
	ledpwm_init();
	mcpwm_init();
	comm_init();
	app_init();

	// Threads
	chThdCreateStatic(periodic_thread_wa, sizeof(periodic_thread_wa), NORMALPRIO, periodic_thread, NULL);
	chThdCreateStatic(sample_send_thread_wa, sizeof(sample_send_thread_wa), NORMALPRIO, sample_send_thread, NULL);
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);

	for(;;) {
		chThdSleepMilliseconds(100);
	}
}
