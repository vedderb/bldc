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

#ifndef APPS_NUNCHUK_NUNCHUK_MANAGER_H_
#define APPS_NUNCHUK_NUNCHUK_MANAGER_H_

#include "apps/common/speed_controller.h"
#include "apps/lighting/led_manager.h"
#include "drivers/nunchuk.h"
#include "util/noncopyable.h"

namespace apps {
namespace nunchuk {

using common::SpeedController;
using drivers::Nunchuk;
using drivers::NunchukEventData;
using lighting::LedManager;

THD_FUNCTION(NunchukManagerThreadFunction, arg);

/*
 * Manages an instance of the Nunchuk driver by handling events and passing
 * messages to other subsystems.
 */
class NunchukManager : public drivers::NunchukEventHandler,
                       public util::NonCopyable {
 public:
  /*
   * Construct a NunchukManager given an i2c device.
   */
  NunchukManager(I2CDriver *i2c_driver, SpeedController *speed_controller,
      LedManager *led_manager);

  /*
   * Starts the Nunchuk event loop.
   */
  void Start();

  /*
   * NunchukEventHandler methods.
   */
  void OnNunchukEvent(Nunchuk *nunchuk,
                      const NunchukEventData& event_data) override;

 private:
  // The underlying nunchuk that is being managed.
  Nunchuk nunchuk_;

  // The speed controller that is being controlled by this NunchukManager.
  SpeedController *speed_controller_;

  // The LED manager that is being controlled by this NunchukManager.
  LedManager *led_manager_;

  // The most recent event provided by the Nunchuk.
  NunchukEventData previous_event_data_;

  // Stillness detection state.
  int still_count_;

  /*
   * Updates the brake lights given a throttle position.
   */
  void UpdateBrakeLights(float throttle_position);

  /*
   * Updates the turn signals given a signal position.
   */
  void UpdateSignalLights(const NunchukEventData& event_data);
};

}  // namespace nunchuk
}  // namespace apps

#endif  // APPS_NUNCHUK_NUNCHUK_MANAGER_H_

