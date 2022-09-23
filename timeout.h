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

#ifndef TIMEOUT_H_
#define TIMEOUT_H_

#include "ch.h"
#include "chtypes.h"
#include "chsystypes.h"
#include "datatypes.h"

#define MAX_THREADS_MONITOR		10
#define MIN_THREAD_ITERATIONS	1

typedef enum {
	THREAD_MCPWM = 0,
	THREAD_CANBUS,
	THREAD_USB,
	THREAD_APP
} WWDT_THREAD_TYPES;

// Functions
void timeout_init(void);
void timeout_configure(systime_t timeout, float brake_current, KILL_SW_MODE kill_sw_mode);
void timeout_reset(void);
bool timeout_has_timeout(void);
float timeout_secs_since_update(void);
bool timeout_kill_sw_active(void);
systime_t timeout_get_timeout_msec(void);
void timeout_configure_IWDT(void);
void timeout_configure_IWDT_slowest(void);
bool timeout_had_IWDG_reset(void);
void timeout_feed_WDT(uint8_t index);
float timeout_get_brake_current(void);
KILL_SW_MODE timeout_get_kill_sw_mode(void);

#endif /* TIMEOUT_H_ */
