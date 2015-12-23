/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

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

#include <stdbool.h>

void utils_step_towards(float *value, float goal, float step);
float utils_calc_ratio(float low, float high, float val);
void utils_norm_angle(float *angle);
void utils_norm_angle_rad(float *angle);
int utils_truncate_number(float *number, float min, float max);
float utils_map(float x, float in_min, float in_max, float out_min, float out_max);
int utils_map_int(int x, int in_min, int in_max, int out_min, int out_max);
void utils_deadband(float *value, float tres, float max);
float utils_angle_difference(float angle1, float angle2);
float utils_angle_difference_rad(float angle1, float angle2);
float utils_avg_angles_rad_fast(float *angles, float *weights, int angles_num);
float utils_middle_of_3(float a, float b, float c);
int utils_middle_of_3_int(int a, int b, int c);
float utils_fast_inv_sqrt(float x);
float utils_fast_atan2(float y, float x);
bool utils_saturate_vector_2d(float *x, float *y, float max);
void utils_fast_sincos(float angle, float *sin, float *cos);
void utils_fast_sincos_better(float angle, float *sin, float *cos);
void utils_sys_lock_cnt(void);
void utils_sys_unlock_cnt(void);

// Return the sign of the argument. -1 if negative, 1 if zero or positive.
#define SIGN(x)				((x<0)?-1:1)

// Return the age of a timestamp in seconds
#define UTILS_AGE_S(x)		((float)chVTTimeElapsedSinceX(x) / (float)CH_CFG_ST_FREQUENCY)

// nan and infinity check for floats
#define UTILS_IS_INF(x)		((x) == (1.0 / 0.0) || (x) == (-1.0 / 0.0))
#define UTILS_IS_NAN(x)		((x) != (x))

/**
 * A simple low pass filter.
 *
 * @param value
 * The filtered value.
 *
 * @param sample
 * Next sample.
 *
 * @param filter_constant
 * Filter constant. Range 0.0 to 1.0, where 1.0 gives the unfiltered value.
 */
#define UTILS_LP_FAST(value, sample, filter_constant)	(value -= (filter_constant) * (value - (sample)))

#endif /* UTILS_H_ */
