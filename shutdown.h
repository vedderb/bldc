/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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

#ifndef SHUTDOWN_H_
#define SHUTDOWN_H_

#include "ch.h"
#include "hal.h"
#include "conf_general.h"

#ifdef HW_SHUTDOWN_HOLD_ON
#define SHUTDOWN_RESET()			shutdown_reset_timer()
#define SHUTDOWN_BUTTON_PRESSED		shutdown_button_pressed()
#else
#define SHUTDOWN_RESET()
#define SHUTDOWN_BUTTON_PRESSED		false
#endif

// Fucntions
void shutdown_init(void);
void shutdown_reset_timer(void);
bool shutdown_button_pressed(void);
float shutdown_get_inactivity_time(void);

#endif /* SHUTDOWN_H_ */
