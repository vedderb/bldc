/*
	Copyright 2024 Benjamin Vedder	benjamin@vedder.se

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

#ifndef DRIVER_PWM_SERVO_H_
#define DRIVER_PWM_SERVO_H_

#include <stdint.h>
#include <stdbool.h>

uint32_t pwm_servo_init(uint32_t freq_hz, float duty);
void pwm_servo_init_servo(void);
void pwm_servo_stop(void);
float pwm_servo_set_duty(float duty);
void pwm_servo_set_servo_out(float output);
bool pwm_servo_is_running(void);

#endif /* DRIVER_PWM_SERVO_H_ */
