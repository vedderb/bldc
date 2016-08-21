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

#ifndef APPS_COMMON_SPEED_CONTROLLER_H_
#define APPS_COMMON_SPEED_CONTROLLER_H_

#include <ch.h>

#include "util/noncopyable.h"

namespace apps {
namespace common {

class SpeedController;

/*
 * Possible directions of the motor.
 */
enum class Direction {
  Forward,
  Reverse,
};

/*
 * The state of the throttle. If the throttle is released, the motors are
 * released.
 */
enum class ThrottleState {
  Released,
  Set,
};

/*
 * The state of the cruise control function.
 */
enum class CruiseControlState {
  Disabled,
  Enabled,
};

/*
 * A speed controller event handler.
 */
class SpeedControllerEventHandler {
 public:
  ~SpeedControllerEventHandler() {}

  /*
   * Invoked when the direction of the motor changes.
   */
  virtual void OnSpeedControllerDirectionChange(SpeedController *controller,
                                                Direction direction) = 0;
};

/*
 * A thread-safe speed controller.
 */
class SpeedController : public util::NonCopyable {
 public:
  /*
   * Setup the initial conditions of the speed controller.
   */
  SpeedController();

  /*
   * Starts the control loop.
   */
  void Start();

  /*
   * Sets the desired throttle position. The speed controller will select
   * a terminal speed and ramp towards it.
   */
  void SetThrottle(ThrottleState throttle_state, float throttle_position);

  /*
   * Sets the state of the cruise control function. When cruise control is
   * enabled, the throttle_position is used to alter the output speed relative
   * to the current, rather than as an absolute value.
   */
  void SetCruiseControlState(CruiseControlState cruise_control_state);

 private:
  // Lock the internal state of the speed controller.
  mutex_t state_mutex_;

  // The position and state of the throttle input.
  float throttle_position_;
  ThrottleState throttle_state_;
  CruiseControlState cruise_control_state_;

  // The current output throttle. Used for ramping.
  float output_throttle_position_;

  // A scaling factor to apply to the throttle input.
  float max_throttle_ratio_;

  // The rate of change of the throttle.
  float throttle_rate_of_change_;

  /*
   * Handles a change in direction by setting the throttle output position to
   * 0.0f. This prevents interpolation through zero, which results in forward
   * acceleration while requesting a negative throttle position (and the
   * opposite as well).
   */
  void HandleDirectionChange();

  /*
   * Ramps the output_throttle_ towards a throttle position.
   */
  void RampTowardsCurrent(float target_current);

  /*
   * Publishes the specified motor current to other VESCs via CAN bus.
   */
  void PublishCurrentToCanBus(float current);

  /*
   * Sets the current of the motor and publishes the current to neighboring
   * VESCs via CAN bus.
   */
  void SetCurrent(float current);
};

}  // namespace common
}  // namespace apps

#endif  // APPS_COMMON_SPEED_CONTROLLER_H_
