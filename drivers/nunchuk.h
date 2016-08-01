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

#ifndef DRIVERS_NUNCHUK_H_
#define DRIVERS_NUNCHUK_H_

#include <cstdint>
#include <hal.h>

#include "util/noncopyable.h"

namespace drivers {

class Nunchuk;

/*
 * The states possible for Nunchuk buttons.
 */
enum class ButtonState : uint8_t {
  Released,
  Pressed,
};

/*
 * An interface to handle Nunchuk events.
 */
class NunchukEventHandler {
 public:
  ~NunchukEventHandler() {}

  /*
   * Invoked when the joystick value has changed.
   */
  virtual void OnNunchukJoystickChange(Nunchuk *nunchuk, int8_t x, int8_t y) {}

  /*
   * Invoked when a 'C' button state change occurs.
   */
  virtual void OnNunchukButtonCChange(Nunchuk *nunchuk, ButtonState state) {}

  /*
   * Invoked when a 'Z' button state change occurs.
   */
  virtual void OnNunchukButtonZChange(Nunchuk *nunchuk, ButtonState state) {}

  /*
   * Invoked when the accelerometer value has changed.
   */
  virtual void OnNunchukAccelChange(Nunchuk* nunchuk,
                                    int16_t x, int16_t y, int16_t z) {}

  /*
   * Invoked when the remote transitions from still to in motion. This happens
   * when an event is published after the maximum number of no-event samples
   * have been taken.
   */
  virtual void OnNunchukInMotion(Nunchuk *nunchuk) {}

  /*
   * Invoked when no events have been published for a maximum number of
   * samples. The remote is assumed to be stationary if this happens.
   */
  virtual void OnNunchukStill(Nunchuk *nunchuk) {}
};

/*
 * A Nintendo Wii Nunchuk driver.
 */
class Nunchuk : public util::NonCopyable {
 public:
  /*
   * Returns the value provided as a float from [-1.0f,1.0f].
   */
  static float NormalizeJoystickValue(int8_t value);

  /*
   * Construct a Nunchuk driver given an i2c driver, event handler, sampling
   * period in milliseconds and a maximum still event counter. When
   * max_still_count consequitive values are detected, stillness is asserted.
   * This can be useful for detecting remote connection loss.
   */
  Nunchuk(I2CDriver *i2c_driver, NunchukEventHandler *event_handler,
          int sample_period_ms, int max_stil_count);

  /*
   * Starts an event loop. Changes in controller state are exposed to a client
   * through the NunchukEventHandler.
   */
  void Start();

 private:
  // The i2c peripheral used to communicate with the Nunchuk receiver.
  I2CDriver *i2c_driver_;

  // The event handler to which state changes are passed.
  NunchukEventHandler *event_handler_;

  // The sampling period of data from the remote.
  int sample_period_ms_;

  // Stillness detector state.
  int still_count_;
  const int max_still_count_;

  // Current state of the joystick for event generation logic.
  int8_t joystick_x_;
  int8_t joystick_y_;
  int16_t accel_x_;
  int16_t accel_y_;
  int16_t accel_z_;
  ButtonState button_c_;
  ButtonState button_z_;

  /*
   * Processes an accel sample and posts an event if necessary. Returns true
   * if an event was generated.
   */
  bool HandleAccelSample(int16_t accel_x, int16_t accel_y, int16_t accel_z);

  /*
   * Processes a C button sample and posts an event if necessary. Returns true
   * if an event was generated.
   */
  bool HandleButtonCSample(ButtonState button_c);

  /*
   * Processes a Z button sample and posts an event if necessary. Returns true
   * if an event was generated.
   */
  bool HandleButtonZSample(ButtonState button_z);

  /*
   * Processes a joystick sample and posts an event if necessary. Returns true
   * if an event was generated.
   */
  bool HandleJoystickSample(int8_t joystick_x, int8_t joystick_y);

  /*
   * Checks for no motion/any motion and posts an event as necessary.
   */
  void HandleStillState(bool event_handled);

  /*
   * Writes the buffer to the nunchuk.
   */
  bool WriteBuffer(const uint8_t *buffer, size_t length);

  /*
   * Reads data into the buffer from the nunchuk.
   */
  bool ReadBuffer(uint8_t *buffer, size_t length);

  /*
   * Sends an initialization sequence to the Nunchuk.
   */
  bool Initialize();
};

}  // namespace drivers

#endif  // DRIVERS_NUNCHUK_H_
