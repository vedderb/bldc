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
 * The data associated with a Nunchuk event.
 */
struct NunchukEventData {
  int8_t joystick_x;
  int8_t joystick_y;
  int16_t accel_x;
  int16_t accel_y;
  int16_t accel_z;
  ButtonState button_c;
  ButtonState button_z;

  bool operator==(const NunchukEventData& event_data) const;
};

/*
 * An interface to handle Nunchuk events.
 */
class NunchukEventHandler {
 public:
  ~NunchukEventHandler() {}

  /*
   * Invoked when event data has been read from the Nunchuk.
   */
  virtual void OnNunchukEvent(Nunchuk *nunchuk,
                              const NunchukEventData& event_data) = 0;
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
          int sample_period_ms);

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
