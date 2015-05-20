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
 * utils.h
 *
 *  Created on: 16 maj 2013
 *      Author: benjamin
 */

#ifndef UTILS_H_
#define UTILS_H_

void utils_step_towards(float *value, float goal, float step);
float utils_calc_ratio(float low, float high, float val);
void utils_norm_angle(float *angle);
int utils_truncate_number(float *number, float min, float max);
float utils_map(float x, float in_min, float in_max, float out_min, float out_max);
void utils_deadband(float *value, float tres, float max);
float utils_angle_difference(float angle1, float angle2);
void utils_sys_lock_cnt(void);
void utils_sys_unlock_cnt(void);

// Return the sign of the argument. -1 if negative, 1 if zero or positive.
#define SIGN(x)				((x<0)?-1:1)

// Return the age of a timestamp in seconds
#define UTILS_AGE_S(x)		((float)chTimeElapsedSince(x) / (float)CH_FREQUENCY)

#endif /* UTILS_H_ */
