/*
	Copyright 2016 - 2020 Benjamin Vedder	benjamin@vedder.se

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

#ifndef LEDPWM_H_
#define LEDPWM_H_

#include "conf_general.h"

#ifdef LED_PWM4_ON
#define HW_LEDPWM_CH		4
#elif defined(LED_PWM3_ON)
#define HW_LEDPWM_CH		3
#elif defined(LED_PWM2_ON)
#define HW_LEDPWM_CH		2
#elif defined(LED_PWM1_ON)
#define HW_LEDPWM_CH		1
#else
#define HW_LEDPWM_CH		0
#endif

// Settings
#define LEDPWM_LED_NUM		(2 + HW_LEDPWM_CH)
#define LEDPWM_CNT_TOP		200

#define LED_GREEN			0
#define LED_RED				1
#define LED_HW1				2
#define LED_HW2				3
#define LED_HW3				4
#define LED_HW4				5

// Functions
void ledpwm_init(void);
void ledpwm_set_intensity(unsigned int led, float intensity);
void ledpwm_led_on(int led);
void ledpwm_led_off(int led);
void ledpwm_update_pwm(void);

#endif /* LEDPWM_H_ */
