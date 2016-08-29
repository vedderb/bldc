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

#include "util/math.h"

#include <cmath>

namespace util {

void ApplyDeadband(float *value, float threshold, float max_value) {
  float abs_value = fabsf(*value);
  if (abs_value < threshold) {
    *value = 0.0f;
    return;
  }

  float deadband_value = ((abs_value - threshold) * max_value)
                             / (max_value - threshold);
  if (*value > 0.0f) {
    *value = deadband_value;
  } else {
    *value = -deadband_value;
  }
}

float Sum(float *buffer, size_t size) {
  float sum = 0.0f;

  for (size_t i = 0; i < size; i++) {
    sum += buffer[i];
  }

  return sum;
}

float Average(float *buffer, size_t size) {
  return Sum(buffer, size) / size;
}

}  // namespace util
