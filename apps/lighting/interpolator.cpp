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

namespace apps {
namespace lighting {

float LinearInterpolate(float start, float end, float progress) {
  return start + ((end - start) * progress);
}

float CubicEaseInInterpolate(float start, float end, float progress) {
  return start + ((end - start) * (progress * progress * progress));
}

float CubicEaseOutInterpolate(float start, float end, float progress) {
  progress -= 1.0f;
  return start + ((end - start) * (((progress * progress * progress) + 1)));
}

float CubicEaseInOutInterpolate(float start, float end, float progress) {
  float cubic_progress;

  if (progress < 0.5f) {
    cubic_progress = 4 * progress * progress * progress;
  } else {
    cubic_progress = (progress - 1) * (2 * progress - 2)
        * (2 * progress - 2) + 1;
  }

  return (start + (end - start)) * cubic_progress;
}

float StepInterpolate(float start, float end, float progress) {
  if (progress < 0.5f) {
    return start;
  } else {
    return end;
  }
}

}  // namespace lighting
}  // namespace apps
