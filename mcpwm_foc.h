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

#ifndef MCPWM_FOC_H_
#define MCPWM_FOC_H_

#include "conf_general.h"
#include "datatypes.h"
#include <stdbool.h>

// Functions
void mcpwm_foc_init(volatile mc_configuration *configuration);
void mcpwm_foc_deinit(void);
bool mcpwm_foc_init_done(void);
void mcpwm_foc_set_configuration(volatile mc_configuration *configuration);
mc_state mcpwm_foc_get_state(void);
bool mcpwm_foc_is_dccal_done(void);
void mcpwm_foc_stop_pwm(void);
void mcpwm_foc_set_duty(float dutyCycle);
void mcpwm_foc_set_duty_noramp(float dutyCycle);
void mcpwm_foc_set_pid_speed(float rpm);
void mcpwm_foc_set_pid_pos(float pos);
void mcpwm_foc_set_current(float current);
void mcpwm_foc_set_brake_current(float current);
void mcpwm_foc_set_handbrake(float current);
void mcpwm_foc_set_openloop(float current, float rpm);
float mcpwm_foc_get_duty_cycle_set(void);
float mcpwm_foc_get_duty_cycle_now(void);
float mcpwm_foc_get_pid_pos_set(void);
float mcpwm_foc_get_pid_pos_now(void);
float mcpwm_foc_get_switching_frequency_now(void);
float mcpwm_foc_get_sampling_frequency_now(void);
float mcpwm_foc_get_rpm(void);
float mcpwm_foc_get_tot_current(void);
float mcpwm_foc_get_tot_current_filtered(void);
float mcpwm_foc_get_abs_motor_current(void);
float mcpwm_foc_get_abs_motor_voltage(void);
float mcpwm_foc_get_abs_motor_current_filtered(void);
float mcpwm_foc_get_tot_current_directional(void);
float mcpwm_foc_get_tot_current_directional_filtered(void);
float mcpwm_foc_get_id(void);
float mcpwm_foc_get_iq(void);
float mcpwm_foc_get_tot_current_in(void);
float mcpwm_foc_get_tot_current_in_filtered(void);
int mcpwm_foc_get_tachometer_value(bool reset);
int mcpwm_foc_get_tachometer_abs_value(bool reset);
float mcpwm_foc_get_phase(void);
float mcpwm_foc_get_phase_observer(void);
float mcpwm_foc_get_phase_encoder(void);
float mcpwm_foc_get_vd(void);
float mcpwm_foc_get_vq(void);
void mcpwm_foc_encoder_detect(float current, bool print, float *offset, float *ratio, bool *inverted);
float mcpwm_foc_measure_resistance(float current, int samples);
float mcpwm_foc_measure_inductance(float duty, int samples, float *curr);
bool mcpwm_foc_measure_res_ind(float *res, float *ind);
bool mcpwm_foc_hall_detect(float current, uint8_t *hall_table);
void mcpwm_foc_print_state(void);
float mcpwm_foc_get_last_inj_adc_isr_duration(void);

// Interrupt handlers
void mcpwm_foc_tim_sample_int_handler(void);
void mcpwm_foc_adc_int_handler(void *p, uint32_t flags);

// Defines
#define MCPWM_FOC_INDUCTANCE_SAMPLE_CNT_OFFSET		10 // Offset for the inductance measurement sample time in timer ticks
#define MCPWM_FOC_INDUCTANCE_SAMPLE_RISE_COMP		50 // Current rise time compensation
#define MCPWM_FOC_I_FILTER_CONST					0.1 // Filter constant for the current filters
#define MCPWM_FOC_CURRENT_SAMP_OFFSET				(2) // Offset from timer top for injected ADC samples

#endif /* MCPWM_FOC_H_ */
