#include "apps/nunchuk/nunchuk_manager.h"

#include "util/math.h"

#include <inttypes.h>
#include "commands.h"

namespace apps {
namespace nunchuk {

using common::ThrottleState;
using drivers::ButtonState;
using lighting::BrakeState;
using lighting::TurnSignalState;

constexpr int kSamplingPeriodMs(10);
constexpr int kMaxStillEventCount(50);
constexpr float kDeadbandFilterThreshold(0.1f);
constexpr float kBrakeThreshold(0.0f);
constexpr float kSignalThreshold(0.9f);

NunchukManager::NunchukManager(I2CDriver *i2c_driver,
                               SpeedController *speed_controller,
                               LedManager *led_manager)
    : nunchuk_(i2c_driver, this, kSamplingPeriodMs),
      speed_controller_(speed_controller),
      led_manager_(led_manager),
      still_count_(0) {
}

void NunchukManager::Start() {
  nunchuk_.Start();
}

void NunchukManager::OnNunchukEvent(Nunchuk *nunchuk,
                                    const NunchukEventData& event_data) {
  // Handle a Nunchuk that appears to be disconnected.
  if (event_data == previous_event_data_) {
    if (still_count_ < kMaxStillEventCount) {
      still_count_++;
    }

    if (still_count_ == kMaxStillEventCount) {
      still_count_ = kMaxStillEventCount + 1;
      speed_controller_->SetThrottle(ThrottleState::Released, 0.0f);
    }

    return;
  }

  // Update the previous event data.
  still_count_ = 0;

  int8_t joystick_y = event_data.joystick_y;
  float throttle_position = Nunchuk::NormalizeJoystickValue(joystick_y);
  util::ApplyDeadband(&throttle_position, kDeadbandFilterThreshold, 1.0f);

  if (event_data.button_z == ButtonState::Released) {
    // Handle release of the dead man switch.
    speed_controller_->SetThrottle(ThrottleState::Released, 0.0f);
  } else if (previous_event_data_.joystick_y != event_data.joystick_y
      || previous_event_data_.button_z == ButtonState::Released) {
    // Handle a new throttle position.
    speed_controller_->SetThrottle(ThrottleState::Set, throttle_position);
  }

  UpdateBrakeLights(throttle_position);
  UpdateSignalLights(event_data);

  previous_event_data_ = event_data;
}

void NunchukManager::UpdateBrakeLights(float throttle_position) {
  if (throttle_position < -kBrakeThreshold) {
    led_manager_->SetBrakeState(BrakeState::Braking);
  } else {
    led_manager_->SetBrakeState(BrakeState::Idle);
  }
}

void NunchukManager::UpdateSignalLights(const NunchukEventData& event_data) {
  int8_t previous_joystick_x = previous_event_data_.joystick_x;
  float previous_signal_position = Nunchuk::NormalizeJoystickValue(
      previous_joystick_x);

  int8_t joystick_x = event_data.joystick_x;
  float signal_position = Nunchuk::NormalizeJoystickValue(joystick_x);

  if (previous_signal_position > -kSignalThreshold
      && signal_position <= -kSignalThreshold) {
    if (led_manager_->turn_signal_state() == TurnSignalState::Disabled) {
      if (event_data.button_c == ButtonState::Pressed
          && event_data.button_z == ButtonState::Released) {
        led_manager_->SetTurnSignalState(TurnSignalState::FourWay);
      } else {
        led_manager_->SetTurnSignalState(TurnSignalState::Left);
      }
    } else if (led_manager_->turn_signal_state() != TurnSignalState::Left) {
      led_manager_->SetTurnSignalState(TurnSignalState::Disabled);
    }
  } else if (previous_signal_position < kSignalThreshold
      && signal_position >= kSignalThreshold) {
    if (led_manager_->turn_signal_state() == TurnSignalState::Disabled) {
      if (event_data.button_c == ButtonState::Pressed
          && event_data.button_z == ButtonState::Released) {
        led_manager_->SetTurnSignalState(TurnSignalState::FourWay);
      } else {
        led_manager_->SetTurnSignalState(TurnSignalState::Right);
      }
    } else if (led_manager_->turn_signal_state() != TurnSignalState::Right) {
      led_manager_->SetTurnSignalState(TurnSignalState::Disabled);
    }
  }
}

}  // namespace nunchuk
}  // namespace apps
