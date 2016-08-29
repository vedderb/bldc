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

#ifndef APPS_LIGHTING_ANIMATION_H_
#define APPS_LIGHTING_ANIMATION_H_

#include <ch.h>
#include <cstdint>

#include "apps/lighting/interpolator.h"
#include "util/noncopyable.h"

namespace apps {
namespace lighting {

/*
 * The possible states that an animation can be in.
 */
enum class AnimationState {
  Stopped,
  Started,
  Uninitialized,
};

/*
 * An generic animation class that manages transitions from one value to
 * another.
 */
template<typename T>
class Animation : public util::NonCopyable {
 public:
  /*
   * The type for the animation step function. A function of this type will be
   * invoked for each frame in an animation.
   */
  typedef void (AnimationUpdateCallback)(float value, T *cookie);

  /*
   * The type for a miscellaneous animation callback function.
   */
  typedef void (AnimationCallback)(T *cookie);

  /*
   * Construct an uninitialized animation.
   */
  Animation();

  /*
   * Construct an animation between two values.
   */
  Animation(float start_value, float end_value,
            AnimationUpdateCallback *update_callback,
            AnimationCallback *complete_callback,
            AnimationCallback *cancel_callback,
            Interpolator *interpolator,
            uint32_t duration_ms,
            T *cookie);

  /*
   * Starts the animation.
   */
  void Start();

  /*
   * Cancels the animation.
   */
  void Cancel();

  /*
   * Steps the animation forward by one frame. Returns true if the animation
   * is currently running.
   */
  bool Step();

  /*
   * Sets the state of the animation to Uninitialized. This animation is
   * considered to be free at this point.
   */
  void Clear();

  /*
   * Returns whether or not the animation has been initilized and is in use.
   */
  bool IsInitialized() const;

  /*
   * Sets the start value of the animation. This should be in the range of 0.0f
   * to 1.0f. This should not be invoked while an animation is playing back.
   * The behavior is not specified.
   */
  void set_start_value(float start_value);

  /*
   * Sets the end value of the animation. This should be in the range of 0.0f
   * to 1.0f. This should not be invoked while an animation is playing back.
   * The behavior is not specified.
   */
  void set_end_value(float end_value);

  /*
   * Sets the duration of the animation in milliseconds. This should not be
   * invoked when an animation is playing back.
   */
  void set_duration_ms(uint32_t duration_ms);

  /*
   * Obtains the value most recently called to the animation step function.
   */
  float value() const;

 private:
  // The start and finish values interpolated between and provided to the
  // update callback.
  float start_value_;
  float end_value_;

  // The step function to invoke for each frame of this animation.
  AnimationUpdateCallback *update_callback_;

  // The complete function to invoke for each frame of this animation.
  AnimationCallback *complete_callback_;

  // The cancel function to invoke when the animation is cancelled.
  AnimationCallback *cancel_callback_;

  // The interpolator to use when stepping the animation forwards.
  Interpolator *interpolator_;

  // The time at which the animation was started.
  systime_t start_time_;

  // The duration of the animation in milliseconds.
  uint32_t duration_ms_;

  // The value most recently passed to the step function.
  float value_;

  // The state of the animation.
  AnimationState state_;

  // The cookie passed into animation callbacks.
  T *cookie_;
};

}  // namespace lighting
}  // namespace apps

#include "apps/lighting/animation_impl.h"

#endif  // APPS_LIGHTING_ANIMATION_H_
