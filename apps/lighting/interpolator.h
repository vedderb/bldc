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

#ifndef APPS_LIGHTING_INTERPOLATOR_H_
#define APPS_LIGHTING_INTERPOLATOR_H_

namespace apps {
namespace lighting {

/*
 * The type for the interpolation function.
 */
typedef float (Interpolator)(float start, float end, float progress);

/*
 * Returns the linearly interpolated value between two values.
 */
float LinearInterpolate(float start, float end, float progress);

/*
 * Returns the interpolated value using a cubic ease in function.
 */
float CubicEaseInInterpolate(float start, float end, float progress);

/*
 * Returns the interpolated value using a cubic ease out function.
 */
float CubicEaseOutInterpolate(float start, float end, float progress);

/*
 * Returns the interpolated value using a cubic ease in/ease out function.
 */
float CubicEaseInOutInterpolate(float start, float end, float progress);

/*
 * Returns a value that is 'start' if progress is <0.5f and 'end' if progress
 * is >= 0.5f.
 */
float StepInterpolate(float start, float end, float progress);

}  // namespace lighting
}  // namespace apps

#endif  // APPS_LIGHTING_INTERPOLATOR_H_
