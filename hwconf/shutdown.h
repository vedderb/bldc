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

#define SHUTDOWN_RESET()					shutdown_reset_timer()

#ifdef HW_SHUTDOWN_HOLD_ON
#define SHUTDOWN_BUTTON_PRESSED				shutdown_button_pressed()
#define SHUTDOWN_SET_SAMPLING_DISABLED(d)	shutdown_set_sampling_disabled(d)
#else
#define SHUTDOWN_BUTTON_PRESSED				false
#define SHUTDOWN_SET_SAMPLING_DISABLED(d)
#endif

#define SHUTDOWN_SAVE_BACKUPDATA_TIMEOUT 60*3 
// time of inactivity after wich backup data (odometer, running time, ...) is
// stored to emulated eeprom when not using power switch. Must be greater than
// average stopping time, usually semaphores require 120s max, so 60*3s or 
// more should be pretty safe 

// Fucntions
void shutdown_init(void);
void shutdown_reset_timer(void);
bool shutdown_button_pressed(void);
float shutdown_get_inactivity_time(void);
void shutdown_set_sampling_disabled(bool disabled);
void shutdown_hold(bool hold);
bool do_shutdown(bool resample);

#endif /* SHUTDOWN_H_ */
