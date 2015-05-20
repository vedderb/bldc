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

/*
 * utils.c
 *
 *  Created on: 16 maj 2013
 *      Author: benjamin
 */

#include "utils.h"
#include "ch.h"
#include "hal.h"
#include <math.h>

// Private variables
static volatile int sys_lock_cnt = 0;

void utils_step_towards(float *value, float goal, float step) {
    if (*value < goal) {
        if ((*value + step) < goal) {
            *value += step;
        } else {
            *value = goal;
        }
    } else if (*value > goal) {
        if ((*value - step) > goal) {
            *value -= step;
        } else {
            *value = goal;
        }
    }
}

float utils_calc_ratio(float low, float high, float val) {
	return (val - low) / (high - low);
}

/**
 * Make sure that 0 <= angle < 360
 * @param angle
 * The angle to normalize.
 */
void utils_norm_angle(float *angle) {
	*angle = fmodf(*angle, 360.0);

	if (*angle < 0.0) {
		*angle += 360.0;
	}
}

int utils_truncate_number(float *number, float min, float max) {
	int did_trunc = 0;

	if (*number > max) {
		*number = max;
		did_trunc = 1;
	} else if (*number < min) {
		*number = min;
		did_trunc = 1;
	}

	return did_trunc;
}

float utils_map(float x, float in_min, float in_max, float out_min, float out_max) {
	return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
}

/**
 * Truncate absolute values less than tres to zero. The value
 * tres will be mapped to 0 and the value max to max.
 */
void utils_deadband(float *value, float tres, float max) {
	if (fabsf(*value) < tres) {
		*value = 0.0;
	} else {
		float k = max / (max - tres);
		if (*value > 0.0) {
			*value = k * *value + max * (1.0 - k);
		} else {
			*value = -(k * -*value + max * (1.0 - k));
		}

	}
}

/**
 * Get the difference between two angles. Will always be between -180 and +180 degrees.
 * @param angle1
 * The first angle
 * @param angle2
 * The second angle
 * @return
 * The difference between the angles
 */
float utils_angle_difference(float angle1, float angle2) {
	utils_norm_angle(&angle1);
	utils_norm_angle(&angle2);

	if (fabsf(angle1 - angle2) > 180.0) {
		if (angle1 < angle2) {
			angle1 += 360.0;
		} else {
			angle2 += 360.0;
		}
	}

	return angle1 - angle2;
}

/**
 * A system locking function with a counter. For every lock, a corresponding unlock must
 * exist to unlock the system. That means, if lock is called five times, unlock has to
 * be called five times as well. Note that chSysLock and chSysLockFromIsr are the same
 * for this port.
 */
void utils_sys_lock_cnt(void) {
	if (!sys_lock_cnt) {
		chSysLock();
	}
	sys_lock_cnt++;
}

/**
 * A system unlocking function with a counter. For every lock, a corresponding unlock must
 * exist to unlock the system. That means, if lock is called five times, unlock has to
 * be called five times as well. Note that chSysUnlock and chSysUnlockFromIsr are the same
 * for this port.
 */
void utils_sys_unlock_cnt(void) {
	if (sys_lock_cnt) {
		sys_lock_cnt--;
		if (!sys_lock_cnt) {
			chSysUnlock();
		}
	}
}
