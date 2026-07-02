/*
    Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef VESC_MOCK_H_
#define VESC_MOCK_H_

#include <stdint.h>
#include <stdbool.h>

typedef struct {
  // --- Sensor fields (testbench writes via sim-set-*, DUT reads via get-*) ---

  // Motor electrical
  float rpm;
  float rpm_fast;
  float rpm_faster;
  float rpm_set;
  float current;
  float current_in;
  float current_dir;
  float duty;
  float duty_abs;
  float vin;
  float id;
  float id_set;
  float id_target;
  float iq;
  float iq_set;
  float iq_target;
  float vd;
  float vq;
  float pos;
  float speed;
  float speed_set;

  // Energy
  float ah;
  float ah_chg;
  float wh;
  float wh_chg;

  // Temperatures
  float temp_fet;
  float temp_mot;
  float temp_mot_res;

  // Battery / distance / PPM
  float batt;
  float dist;
  float dist_abs;
  float ppm;
  float ppm_age;

  // ADC (4 channels each)
  float adc[4];
  float adc_decoded[4];

  // Encoder
  float encoder;
  float encoder_error_rate;
  bool  encoder_index_found;

  // Phase
  float phase_encoder;
  float phase_hall;
  float phase_motor;
  float phase_observer;

  // FOC / observer (get-est-* and foc-est-* are aliases to the same fields)
  float est_ind;
  float est_lambda;
  float est_res;
  float hfi_res;
  float observer_error;

  // Raw modulation
  float mod_alpha;
  float mod_beta;
  float mod_alpha_measured;
  float mod_beta_measured;

  // IMU
  float imu_acc[3];
  float imu_acc_derot[3];
  float imu_gyro[3];
  float imu_gyro_derot[3];
  float imu_mag[3];
  float imu_rpy[3];
  float imu_quat[4];

  // GNSS
  float gnss_lat;
  float gnss_lon;
  float gnss_height;
  float gnss_speed;
  float gnss_hdop;
  float gnss_age;

  // Environmental (BME280)
  float bme280_temp;
  float bme280_hum;
  float bme280_pres;

  // AS504x magnetic encoder
  float as504x_angle;

  // GPIO (by integer index 0-15)
  bool gpio[16];

  // PID state
  float pos_pid_error;
  float pos_pid_now;
  float pos_pid_set;

  // App state
  float app_pas_rpm;
  bool  app_is_output_disabled;
  bool  app_adc_range_ok;

  // Connection status
  bool connected_ble;
  bool connected_hub;
  bool connected_usb;
  bool connected_wifi;

  // Multi-motor setup
  float   setup_current;
  float   setup_current_in;
  float   setup_ah;
  float   setup_ah_chg;
  float   setup_wh;
  float   setup_wh_chg;
  int32_t setup_num_vescs;

  // System
  int32_t selected_motor;
  char    fault_name[32];

  // --- Command fields (DUT writes via set-*, testbench reads via sim-get-*) ---
  float   cmd_current;
  float   cmd_current_rel;
  float   cmd_duty;
  float   cmd_rpm;
  float   cmd_pos;
  float   cmd_brake;
  float   cmd_brake_rel;
  float   cmd_handbrake;
  float   cmd_handbrake_rel;
  float   cmd_servo;
  float   cmd_encoder;
  int32_t cmd_selected_motor;
  bool    cmd_kill_sw;
} vesc_mock_state_t;

// External simulator interface: write sensor values in, read commanded values out.
void vesc_mock_write(const vesc_mock_state_t *state);
void vesc_mock_read(vesc_mock_state_t *state);

#endif
