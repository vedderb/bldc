/*
 * Copyright 2016 Andrew Rossignol andrew.rossignol@gmail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef UTIL_MATH_H_
#define UTIL_MATH_H_

#include <cstddef>

namespace util {

/*
 * Floors values less than threshold to zero. Scales values between threshold
 * and max_value from 0 to max_value.
 */
void ApplyDeadband(float *value, float threshold, float max_value);

/*
 * Compute the summation of the buffer.
 */
float Sum(float *buffer, size_t size);

/*
 * Compute the average of the buffer.
 */
float Average(float *buffer, size_t size);

}  // namespace util

#endif  // UTIL_MATH_H_
