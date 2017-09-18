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

#include "utils.h"
#include "ch.h"
#include "hal.h"
#include <math.h>
#include <string.h>

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
 *
 * @param angle
 * The angle to normalize.
 */
void utils_norm_angle(float *angle) {
	*angle = fmodf(*angle, 360.0);

	if (*angle < 0.0) {
		*angle += 360.0;
	}
}

/**
 * Make sure that -pi <= angle < pi,
 *
 * TODO: Maybe use fmodf instead?
 *
 * @param angle
 * The angle to normalize in radians.
 * WARNING: Don't use too large angles.
 */
void utils_norm_angle_rad(float *angle) {
	while (*angle < -M_PI) {
		*angle += 2.0 * M_PI;
	}

	while (*angle >  M_PI) {
		*angle -= 2.0 * M_PI;
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

int utils_truncate_number_int(int *number, int min, int max) {
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

int utils_truncate_number_abs(float *number, float max) {
	int did_trunc = 0;

	if (*number > max) {
		*number = max;
		did_trunc = 1;
	} else if (*number < -max) {
		*number = -max;
		did_trunc = 1;
	}

	return did_trunc;
}

float utils_map(float x, float in_min, float in_max, float out_min, float out_max) {
	return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
}

int utils_map_int(int x, int in_min, int in_max, int out_min, int out_max) {
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
//	utils_norm_angle(&angle1);
//	utils_norm_angle(&angle2);
//
//	if (fabsf(angle1 - angle2) > 180.0) {
//		if (angle1 < angle2) {
//			angle1 += 360.0;
//		} else {
//			angle2 += 360.0;
//		}
//	}
//
//	return angle1 - angle2;

	// Faster in most cases
	float difference = angle1 - angle2;
	while (difference < -180.0) difference += 2.0 * 180.0;
	while (difference > 180.0) difference -= 2.0 * 180.0;
	return difference;
}

/**
 * Get the difference between two angles. Will always be between -pi and +pi radians.
 * @param angle1
 * The first angle in radians
 * @param angle2
 * The second angle in radians
 * @return
 * The difference between the angles in radians
 */
float utils_angle_difference_rad(float angle1, float angle2) {
	float difference = angle1 - angle2;
	while (difference < -M_PI) difference += 2.0 * M_PI;
	while (difference > M_PI) difference -= 2.0 * M_PI;
	return difference;
}

/**
 * Takes the average of a number of angles.
 *
 * @param angles
 * The angles in radians.
 *
 * @param angles_num
 * The number of angles.
 *
 * @param weights
 * The weight of the summarized angles
 *
 * @return
 * The average angle.
 */
float utils_avg_angles_rad_fast(float *angles, float *weights, int angles_num) {
	float s_sum = 0.0;
	float c_sum = 0.0;

	for (int i = 0; i < angles_num; i++) {
		float s, c;
		utils_fast_sincos_better(angles[i], &s, &c);
		s_sum += s * weights[i];
		c_sum += c * weights[i];
	}

	return utils_fast_atan2(s_sum, c_sum);
}

/**
 * Get the middle value of three values
 *
 * @param a
 * First value
 *
 * @param b
 * Second value
 *
 * @param c
 * Third value
 *
 * @return
 * The middle value
 */
float utils_middle_of_3(float a, float b, float c) {
	float middle;

	if ((a <= b) && (a <= c)) {
		middle = (b <= c) ? b : c;
	} else if ((b <= a) && (b <= c)) {
		middle = (a <= c) ? a : c;
	} else {
		middle = (a <= b) ? a : b;
	}
	return middle;
}

/**
 * Get the middle value of three values
 *
 * @param a
 * First value
 *
 * @param b
 * Second value
 *
 * @param c
 * Third value
 *
 * @return
 * The middle value
 */
int utils_middle_of_3_int(int a, int b, int c) {
	int middle;

	if ((a <= b) && (a <= c)) {
		middle = (b <= c) ? b : c;
	} else if ((b <= a) && (b <= c)) {
		middle = (a <= c) ? a : c;
	} else {
		middle = (a <= b) ? a : b;
	}
	return middle;
}

// Fast inverse square-root
// See: http://en.wikipedia.org/wiki/Fast_inverse_square_root
float utils_fast_inv_sqrt(float x) {
	union {
		float as_float;
		long as_int;
	} un;

	float xhalf = 0.5f*x;
	un.as_float = x;
	un.as_int = 0x5f3759df - (un.as_int >> 1);
	un.as_float = un.as_float * (1.5f - xhalf * un.as_float * un.as_float);
	return un.as_float;
}

/**
 * Fast atan2
 *
 * See http://www.dspguru.com/dsp/tricks/fixed-point-atan2-with-self-normalization
 *
 * @param y
 * y
 *
 * @param x
 * x
 *
 * @return
 * The angle in radians
 */
float utils_fast_atan2(float y, float x) {
	float abs_y = fabsf(y) + 1e-20; // kludge to prevent 0/0 condition
	float angle;

	if (x >= 0) {
		float r = (x - abs_y) / (x + abs_y);
		float rsq = r * r;
		angle = ((0.1963 * rsq) - 0.9817) * r + (M_PI / 4.0);
	} else {
		float r = (x + abs_y) / (abs_y - x);
		float rsq = r * r;
		angle = ((0.1963 * rsq) - 0.9817) * r + (3.0 * M_PI / 4.0);
	}

	if (y < 0) {
		return(-angle);
	} else {
		return(angle);
	}
}

/**
 * Truncate the magnitude of a vector.
 *
 * @param x
 * The first component.
 *
 * @param y
 * The second component.
 *
 * @param max
 * The maximum magnitude.
 *
 * @return
 * True if saturation happened, false otherwise
 */
bool utils_saturate_vector_2d(float *x, float *y, float max) {
	bool retval = false;
	float mag = sqrtf(*x * *x + *y * *y);
	max = fabsf(max);

	if (mag < 1e-10) {
		mag = 1e-10;
	}

	if (mag > max) {
		const float f = max / mag;
		*x *= f;
		*y *= f;
		retval = true;
	}

	return retval;
}

/**
 * Fast sine and cosine implementation.
 *
 * See http://lab.polygonal.de/?p=205
 *
 * @param angle
 * The angle in radians
 * WARNING: Don't use too large angles.
 *
 * @param sin
 * A pointer to store the sine value.
 *
 * @param cos
 * A pointer to store the cosine value.
 */
void utils_fast_sincos(float angle, float *sin, float *cos) {
	//always wrap input angle to -PI..PI
	while (angle < -M_PI) {
		angle += 2.0 * M_PI;
	}

	while (angle >  M_PI) {
		angle -= 2.0 * M_PI;
	}

	// compute sine
	if (angle < 0.0) {
		*sin = 1.27323954 * angle + 0.405284735 * angle * angle;
	} else {
		*sin = 1.27323954 * angle - 0.405284735 * angle * angle;
	}

	// compute cosine: sin(x + PI/2) = cos(x)
	angle += 0.5 * M_PI;

	if (angle >  M_PI) {
		angle -= 2.0 * M_PI;
	}

	if (angle < 0.0) {
		*cos = 1.27323954 * angle + 0.405284735 * angle * angle;
	} else {
		*cos = 1.27323954 * angle - 0.405284735 * angle * angle;
	}
}

/**
 * Fast sine and cosine implementation.
 *
 * See http://lab.polygonal.de/?p=205
 *
 * @param angle
 * The angle in radians
 * WARNING: Don't use too large angles.
 *
 * @param sin
 * A pointer to store the sine value.
 *
 * @param cos
 * A pointer to store the cosine value.
 */
void utils_fast_sincos_better(float angle, float *sin, float *cos) {
	//always wrap input angle to -PI..PI
	while (angle < -M_PI) {
		angle += 2.0 * M_PI;
	}

	while (angle >  M_PI) {
		angle -= 2.0 * M_PI;
	}

	//compute sine
	if (angle < 0.0) {
		*sin = 1.27323954 * angle + 0.405284735 * angle * angle;

		if (*sin < 0.0) {
			*sin = 0.225 * (*sin * -*sin - *sin) + *sin;
		} else {
			*sin = 0.225 * (*sin * *sin - *sin) + *sin;
		}
	} else {
		*sin = 1.27323954 * angle - 0.405284735 * angle * angle;

		if (*sin < 0.0) {
			*sin = 0.225 * (*sin * -*sin - *sin) + *sin;
		} else {
			*sin = 0.225 * (*sin * *sin - *sin) + *sin;
		}
	}

	// compute cosine: sin(x + PI/2) = cos(x)
	angle += 0.5 * M_PI;
	if (angle >  M_PI) {
		angle -= 2.0 * M_PI;
	}

	if (angle < 0.0) {
		*cos = 1.27323954 * angle + 0.405284735 * angle * angle;

		if (*cos < 0.0) {
			*cos = 0.225 * (*cos * -*cos - *cos) + *cos;
		} else {
			*cos = 0.225 * (*cos * *cos - *cos) + *cos;
		}
	} else {
		*cos = 1.27323954 * angle - 0.405284735 * angle * angle;

		if (*cos < 0.0) {
			*cos = 0.225 * (*cos * -*cos - *cos) + *cos;
		} else {
			*cos = 0.225 * (*cos * *cos - *cos) + *cos;
		}
	}
}

/**
 * Calculate the values with the lowest magnitude.
 *
 * @param va
 * The first value.
 *
 * @param vb
 * The second value.
 *
 * @return
 * The value with the lowest magnitude.
 */
float utils_min_abs(float va, float vb) {
	float res;
	if (fabsf(va) < fabsf(vb)) {
		res = va;
	} else {
		res = vb;
	}

	return res;
}

/**
 * Calculate the values with the highest magnitude.
 *
 * @param va
 * The first value.
 *
 * @param vb
 * The second value.
 *
 * @return
 * The value with the highest magnitude.
 */
float utils_max_abs(float va, float vb) {
	float res;
	if (fabsf(va) > fabsf(vb)) {
		res = va;
	} else {
		res = vb;
	}

	return res;
}

/**
 * Create string representation of the binary content of a byte
 *
 * @param x
 * The byte.
 *
 * @param b
 * Array to store the string representation in.
 */
void utils_byte_to_binary(int x, char *b) {
	b[0] = '\0';

	int z;
	for (z = 128; z > 0; z >>= 1) {
		strcat(b, ((x & z) == z) ? "1" : "0");
	}
}

float utils_throttle_curve(float val, float curve_acc, float curve_brake, int mode) {
	float ret = 0.0;
	
	if (val < -1.0) {
		val = -1.0;
	}

	if (val > 1.0) {
		val = 1.0;
	}
	
	float val_a = fabsf(val);

	float curve;
	if (val >= 0.0) {
		curve = curve_acc;
	} else {
		curve = curve_brake;
	}

	// See
	// http://math.stackexchange.com/questions/297768/how-would-i-create-a-exponential-ramp-function-from-0-0-to-1-1-with-a-single-val
	if (mode == 0) { // Exponential
		if (curve >= 0.0) {
			ret = 1.0 - powf(1.0 - val_a, 1.0 + curve);
		} else {
			ret = powf(val_a, 1.0 - curve);
		}
	} else if (mode == 1) { // Natural
		if (fabsf(curve) < 1e-10) {
			ret = val_a;
		} else {
			if (curve >= 0.0) {
				ret = 1.0 - ((expf(curve * (1.0 - val_a)) - 1.0) / (expf(curve) - 1.0));
			} else {
				ret = (expf(-curve * val_a) - 1.0) / (expf(-curve) - 1.0);
			}
		}
	} else if (mode == 2) { // Polynomial
		if (curve >= 0.0) {
			ret = 1.0 - ((1.0 - val_a) / (1.0 + curve * val_a));
		} else {
			ret = val_a / (1.0 - curve * (1.0 - val_a));
		}
	} else { // Linear
		ret = val_a;
	}

	if (val < 0.0) {
		ret = -ret;
	}

	return ret;
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
