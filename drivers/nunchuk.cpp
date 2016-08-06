#include "drivers/nunchuk.h"

namespace drivers {

// Maximum time allowed for an i2c transaction.
constexpr int kNunchukTImeoutMilliseconds(5);

// The address of the nunchuk controller.
constexpr i2caddr_t kNunchukAddress(0x52);

// The maximum value for the joystick axis.
constexpr int kMaxJoystickValue(127);

float Nunchuk::NormalizeJoystickValue(int8_t value) {
  return static_cast<float>(value) / kMaxJoystickValue;
}

bool NunchukEventData::operator==(const NunchukEventData& event_data) const {
  return event_data.joystick_x == joystick_x
    && event_data.joystick_y == joystick_y
    && event_data.accel_x == accel_x
    && event_data.accel_y == accel_y
    && event_data.accel_z == accel_z
    && event_data.button_c == button_c
    && event_data.button_z == button_z;
}

Nunchuk::Nunchuk(I2CDriver *i2c_driver, NunchukEventHandler *event_handler,
                 int sample_period_ms)
    : i2c_driver_(i2c_driver),
      event_handler_(event_handler),
      sample_period_ms_(sample_period_ms) {
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
      uint8_t response[6];
      if (!ReadBuffer(response, sizeof(response))) {
        break;
      }

      NunchukEventData event_data = {
        .joystick_x = static_cast<int8_t>((response[0] + 128)),
        .joystick_y = static_cast<int8_t>((response[1] + 128)),
        .accel_x = static_cast<int16_t>(((response[2] << 2)
              | ((response[5] >> 2) & 0x03)) - 512),
        .accel_y = static_cast<int16_t>(((response[3] << 2)
              | ((response[5] >> 5) & 0x03)) - 512),
        .accel_z = static_cast<int16_t>(((response[4] << 2)
              | ((response[5] >> 6) & 0x03)) - 512),
        .button_c = ParseButtonState(response[5], 1),
        .button_z = ParseButtonState(response[5], 0),
      };

      // Post the event.
      event_handler_->OnNunchukEvent(this, event_data);
  
      // Sleep for the sampling interval.
      chThdSleepMilliseconds(sample_period_ms_);
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
