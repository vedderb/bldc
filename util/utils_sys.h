/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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

#ifndef UTILS_SYS_H_
#define UTILS_SYS_H_

#include <stdbool.h>
#include <stdint.h>
#include "datatypes.h"

void utils_sys_lock_cnt(void);
void utils_sys_unlock_cnt(void);
uint8_t utils_second_motor_id(void);
int utils_read_hall(bool is_second_motor, int samples);
const char* utils_hw_type_to_string(HW_TYPE hw);
int utils_check_min_stack_left(thread_t *th);
int utils_stack_left_now(void);
bool utils_is_func_valid(void *addr);

// Return the age of a timestamp in seconds
#define UTILS_AGE_S(x)		((float)chVTTimeElapsedSinceX(x) / (float)CH_CFG_ST_FREQUENCY)

#endif  /* UTILS_SYS_H_ */
