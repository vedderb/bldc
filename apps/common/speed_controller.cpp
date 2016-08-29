#include "apps/common/speed_controller.h"

#include <cmath>

#include "util/singleton.h"

#include "comm_can.h"
#include "mc_interface.h"
#include "timeout.h"
#include "utils.h"

#define MAX_CAN_AGE 0.1

namespace apps {
namespace common {

THD_FUNCTION(SpeedControllerThreadFunction, arg) {
  util::Singleton<SpeedController>::Instance()->Start();
}

// The current applied to the motors when released.
constexpr float kMotorsReleasedCurrent(0.0f);

// The ratio of brake current applied to the motors when the motors are engaged
// with the throttle position set to zero.
constexpr float kIdleBrakeThrottlePosition(0.1f);

SpeedController::SpeedController()
    : cruise_control_state_(CruiseControlState::Disabled) {
  chMtxObjectInit(&state_mutex_);
  SetThrottle(ThrottleState::Released, 0.0f);

  max_throttle_ratio_ = 1.0f;
  throttle_rate_of_change_ = 0.005f;
}

void SpeedController::Start() {
  const volatile mc_configuration *mcconf = mc_interface_get_configuration();

  while (1) {
    chMtxLock(&state_mutex_);

    if (cruise_control_state_ == CruiseControlState::Enabled) {
      // The behavior is different when cruise control is enabled.
    } else if (throttle_state_ == ThrottleState::Released) {
      SetCurrent(kMotorsReleasedCurrent);
    } else {
      // The throttle is engaged so we ramp towards the output throttle
      // position.
      HandleDirectionChange();
      RampTowardsCurrent(throttle_position_);
      float current = output_throttle_position_ * mcconf->l_current_max;
      SetCurrent(current);
    }

    chMtxUnlock(&state_mutex_);
    chThdSleepMilliseconds(10);
  }
}

void SpeedController::SetThrottle(ThrottleState throttle_state,
                                  float throttle_position) {
  chMtxLock(&state_mutex_);

  throttle_state_ = throttle_state;
  throttle_position_ = throttle_position;

  if (throttle_state == ThrottleState::Released) {
    throttle_position_ = 0.0f;
    output_throttle_position_ = 0.0f;
  }

  chMtxUnlock(&state_mutex_);
}

void SpeedController::SetCruiseControlState(
    CruiseControlState cruise_control_state) {
  chMtxLock(&state_mutex_);
  cruise_control_state_ = cruise_control_state;
  chMtxUnlock(&state_mutex_);
}

void SpeedController::HandleDirectionChange() {
  if ((output_throttle_position_ > 0.0f && throttle_position_ < 0.0f)
      || (output_throttle_position_ < 0.0f && throttle_position_ > 0.0f)) {
    output_throttle_position_ = 0.0f;
  }
}

void SpeedController::RampTowardsCurrent(float throttle_position) {
  float limited_throttle_position = max_throttle_ratio_ * throttle_position;
  float throttle_delta = output_throttle_position_ - limited_throttle_position;
  if (fabsf(throttle_delta) < throttle_rate_of_change_) {
    output_throttle_position_ = limited_throttle_position;
  } else if (throttle_delta < 0) {
    output_throttle_position_ += throttle_rate_of_change_;
  } else {
    output_throttle_position_ -= throttle_rate_of_change_;
  }
}

void SpeedController::PublishCurrentToCanBus(float current) {
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    can_status_msg *msg = comm_can_get_status_msg_index(i);

    if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
      comm_can_set_current(msg->id, current);
    }
  }
}

void SpeedController::SetCurrent(float current) {
  PublishCurrentToCanBus(current);
  mc_interface_set_current(current);

  // The motors must be updated at a minimum rate before a timeout occurs.
  // Reset that timeout.
  timeout_reset();
}

}  // namespace common
}  // namespace apps

