#include "drivers/nunchuk.h"

namespace drivers {

// Maximum time allowed for an i2c transaction.
constexpr int kNunchukTImeoutMilliseconds(5);

// The address of the nunchuk controller.
constexpr i2caddr_t kNunchukAddress(0x52);

Nunchuk::Nunchuk(I2CDriver *i2c_driver, NunchukEventHandler *event_handler,
                 int sample_period_ms, int max_still_count)
    : i2c_driver_(i2c_driver),
      event_handler_(event_handler),
      sample_period_ms_(sample_period_ms),
      still_count_(0),
      max_still_count_(max_still_count) {
}

/*
 * Obtains the button state from a given byte and a bit position.
 */
constexpr ButtonState ParseButtonState(uint8_t byte, uint8_t bit_pos) {
  return static_cast<ButtonState>(((byte >> bit_pos) & 0x01) ^ 0x01);
}

void Nunchuk::Start() {
  while (1) {
    // Try to initialize until it succeeds. This can happen if the remote is
    // not connected wirelessly.
    while (!Initialize()) {
      chThdSleepMilliseconds(100);
    }
  
    while (1) {
      // Issue a request for a sample.
      const uint8_t read_command[1] = { 0x00 };
      if (!WriteBuffer(read_command, sizeof(read_command))) {
        break;
      }
  
      // Read the sample into a buffer.
      uint8_t response[7];
      if (!ReadBuffer(response, sizeof(response))) {
        break;
      }
  
      // Parse the buffer into the various parameters of the nunchuk.
      int8_t joystick_x = (response[0] + 128);
      int8_t joystick_y = (response[1] + 128);
      int16_t accel_x = ((response[2] << 2) | ((response[5] >> 2) & 0x03));
      int16_t accel_y = ((response[3] << 2) | ((response[5] >> 5) & 0x03));
      int16_t accel_z = ((response[4] << 2) | ((response[5] >> 6) & 0x03));
      ButtonState button_c = ParseButtonState(response[5], 1);
      ButtonState button_z = ParseButtonState(response[5], 0);
  
      // Handle and post events.
      bool event_handled = HandleAccelSample(accel_x, accel_y, accel_z);
      event_handled |= HandleButtonCSample(button_c);
      event_handled |= HandleButtonZSample(button_z);
      event_handled |= HandleJoystickSample(joystick_x, joystick_y);
      HandleStillState(event_handled);
  
      // Sleep for the sampling interval.
      chThdSleepMilliseconds(sample_period_ms_);
    }
  }
}

bool Nunchuk::HandleAccelSample(int16_t accel_x, int16_t accel_y,
                                int16_t accel_z) {
  if (accel_x != accel_x_ || accel_y != accel_y_ || accel_z != accel_z_) {
    event_handler_->OnNunchukAccelChange(this, accel_x, accel_y, accel_z);
    accel_x_ = accel_x;
    accel_y_ = accel_y;
    accel_z_ = accel_z;
    return true;
  }

  return false;
}

bool Nunchuk::HandleButtonCSample(ButtonState button_c) {
  if (button_c != button_c_) {
    event_handler_->OnNunchukButtonCChange(this, button_c);
    button_c_ = button_c;
    return true;
  }

  return false;
}

bool Nunchuk::HandleButtonZSample(ButtonState button_z) {
  if (button_z != button_z_) {
    event_handler_->OnNunchukButtonZChange(this, button_z);
    button_z_ = button_z;
    return true;
  }

  return false;
}

bool Nunchuk::HandleJoystickSample(int8_t joystick_x, int8_t joystick_y) {
  if (joystick_x != joystick_x_ || joystick_y != joystick_y_) {
    event_handler_->OnNunchukJoystickChange(this, joystick_x, joystick_y);
    joystick_x_ = joystick_x;
    joystick_y_ = joystick_y;
    return true;
  }

  return false;
}

void Nunchuk::HandleStillState(bool event_handled) {
  if (event_handled) {
    if (still_count_ == max_still_count_) {
      event_handler_->OnNunchukInMotion(this);
    }

    still_count_ = 0;
  } else {
    if (still_count_ == (max_still_count_ - 1)) {
      event_handler_->OnNunchukStill(this);
    }

    still_count_++;
    if (still_count_ >= max_still_count_) {
      still_count_ = max_still_count_;
    }
  }
}

bool Nunchuk::WriteBuffer(const uint8_t *buffer, size_t length) {
  i2cAcquireBus(i2c_driver_);
  msg_t status = i2cMasterTransmitTimeout(i2c_driver_, kNunchukAddress,
                                          buffer, length, nullptr, 0,
                                          MS2ST(kNunchukTImeoutMilliseconds));
  i2cReleaseBus(i2c_driver_);

  return status == MSG_OK;
}

bool Nunchuk::ReadBuffer(uint8_t *buffer, size_t length) {
   i2cAcquireBus(i2c_driver_);
  msg_t status = i2cMasterReceiveTimeout(i2c_driver_, kNunchukAddress,
                                         buffer, length,
                                         MS2ST(kNunchukTImeoutMilliseconds));
  i2cReleaseBus(i2c_driver_);

  return status == MSG_OK;
 
}

bool Nunchuk::Initialize() {
  const uint8_t init_command[2][2] = {
    { 0xF0, 0x55, },
    { 0xFB, 0x00, },
  };

  for (size_t i = 0; i < sizeof(init_command) / sizeof(init_command[0]); i++) {
    if (!WriteBuffer(init_command[i], sizeof(init_command[i]))) {
      return false;
    }
  }

  return true;
}

}  // namespace drivers
