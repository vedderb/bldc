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

#include "lispbm.h"
#include "platform_mutex.h"
#include "vesc_mock.h"

#include <stdlib.h>
#include <string.h>

// ---------------------------------------------------------------------------
// Shared mock state and mutex
// ---------------------------------------------------------------------------

static vesc_mock_state_t mock_state;
static lbm_mutex_t       mock_mutex;

void vesc_mock_write(const vesc_mock_state_t *state) {
  lbm_mutex_lock(&mock_mutex);
  memcpy(&mock_state, state, sizeof(vesc_mock_state_t));
  lbm_mutex_unlock(&mock_mutex);
}

void vesc_mock_read(vesc_mock_state_t *state) {
  lbm_mutex_lock(&mock_mutex);
  memcpy(state, &mock_state, sizeof(vesc_mock_state_t));
  lbm_mutex_unlock(&mock_mutex);
}

// ---------------------------------------------------------------------------
// Nil stub for extensions that have no meaningful mock value
// ---------------------------------------------------------------------------

static lbm_value ext_stub(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_NIL;
}

// ---------------------------------------------------------------------------
// SENSOR_FLOAT: single float sensor
//   DUT reads:       (get-X)        -> lbm_enc_float(mock_state.field)
//   Testbench sets:  (sim-set-X v)  -> writes mock_state.field
//
// Table columns: (lbm_get_name, lbm_sim_name, c_getter, c_setter, field)
// ---------------------------------------------------------------------------

#define SENSOR_FLOAT_LIST \
  X("get-rpm",                "sim-set-rpm",                get_rpm,                sim_set_rpm,                rpm) \
  X("get-rpm-fast",           "sim-set-rpm-fast",           get_rpm_fast,           sim_set_rpm_fast,           rpm_fast) \
  X("get-rpm-faster",         "sim-set-rpm-faster",         get_rpm_faster,         sim_set_rpm_faster,         rpm_faster) \
  X("get-rpm-set",            "sim-set-rpm-set",            get_rpm_set,            sim_set_rpm_set,            rpm_set) \
  X("get-current",            "sim-set-current",            get_current,            sim_set_current,            current) \
  X("get-current-in",         "sim-set-current-in",         get_current_in,         sim_set_current_in,         current_in) \
  X("get-current-dir",        "sim-set-current-dir",        get_current_dir,        sim_set_current_dir,        current_dir) \
  X("get-duty",               "sim-set-duty",               get_duty,               sim_set_duty,               duty) \
  X("get-duty-abs",           "sim-set-duty-abs",           get_duty_abs,           sim_set_duty_abs,           duty_abs) \
  X("get-vin",                "sim-set-vin",                get_vin,                sim_set_vin,                vin) \
  X("get-id",                 "sim-set-id",                 get_id,                 sim_set_id,                 id) \
  X("get-id-set",             "sim-set-id-set",             get_id_set,             sim_set_id_set,             id_set) \
  X("get-id-target",          "sim-set-id-target",          get_id_target,          sim_set_id_target,          id_target) \
  X("get-iq",                 "sim-set-iq",                 get_iq,                 sim_set_iq,                 iq) \
  X("get-iq-set",             "sim-set-iq-set",             get_iq_set,             sim_set_iq_set,             iq_set) \
  X("get-iq-target",          "sim-set-iq-target",          get_iq_target,          sim_set_iq_target,          iq_target) \
  X("get-vd",                 "sim-set-vd",                 get_vd,                 sim_set_vd,                 vd) \
  X("get-vq",                 "sim-set-vq",                 get_vq,                 sim_set_vq,                 vq) \
  X("get-pos",                "sim-set-pos",                get_pos,                sim_set_pos,                pos) \
  X("get-speed",              "sim-set-speed",              get_speed,              sim_set_speed,              speed) \
  X("get-speed-set",          "sim-set-speed-set",          get_speed_set,          sim_set_speed_set,          speed_set) \
  X("get-ah",                 "sim-set-ah",                 get_ah,                 sim_set_ah,                 ah) \
  X("get-ah-chg",             "sim-set-ah-chg",             get_ah_chg,             sim_set_ah_chg,             ah_chg) \
  X("get-wh",                 "sim-set-wh",                 get_wh,                 sim_set_wh,                 wh) \
  X("get-wh-chg",             "sim-set-wh-chg",             get_wh_chg,             sim_set_wh_chg,             wh_chg) \
  X("get-temp-fet",           "sim-set-temp-fet",           get_temp_fet,           sim_set_temp_fet,           temp_fet) \
  X("get-temp-mot",           "sim-set-temp-mot",           get_temp_mot,           sim_set_temp_mot,           temp_mot) \
  X("get-temp-mot-res",       "sim-set-temp-mot-res",       get_temp_mot_res,       sim_set_temp_mot_res,       temp_mot_res) \
  X("get-batt",               "sim-set-batt",               get_batt,               sim_set_batt,               batt) \
  X("get-dist",               "sim-set-dist",               get_dist,               sim_set_dist,               dist) \
  X("get-dist-abs",           "sim-set-dist-abs",           get_dist_abs,           sim_set_dist_abs,           dist_abs) \
  X("get-ppm",                "sim-set-ppm",                get_ppm,                sim_set_ppm,                ppm) \
  X("get-ppm-age",            "sim-set-ppm-age",            get_ppm_age,            sim_set_ppm_age,            ppm_age) \
  X("get-encoder",            "sim-set-encoder",            get_encoder,            sim_set_encoder,            encoder) \
  X("get-encoder-error-rate", "sim-set-encoder-error-rate", get_encoder_error_rate, sim_set_encoder_error_rate, encoder_error_rate) \
  X("get-est-ind",            "sim-set-est-ind",            get_est_ind,            sim_set_est_ind,            est_ind) \
  X("get-est-lambda",         "sim-set-est-lambda",         get_est_lambda,         sim_set_est_lambda,         est_lambda) \
  X("get-est-res",            "sim-set-est-res",            get_est_res,            sim_set_est_res,            est_res) \
  X("get-hfi-res",            "sim-set-hfi-res",            get_hfi_res,            sim_set_hfi_res,            hfi_res) \
  X("observer-error",         "sim-set-observer-error",     observer_error,         sim_set_observer_error,     observer_error) \
  X("pos-pid-error",          "sim-set-pos-pid-error",      pos_pid_error,          sim_set_pos_pid_error,      pos_pid_error) \
  X("pos-pid-now",            "sim-set-pos-pid-now",        pos_pid_now,            sim_set_pos_pid_now,        pos_pid_now) \
  X("pos-pid-set",            "sim-set-pos-pid-set",        pos_pid_set,            sim_set_pos_pid_set,        pos_pid_set) \
  X("as504x-angle",           "sim-set-as504x-angle",       as504x_angle,           sim_set_as504x_angle,       as504x_angle) \
  X("app-pas-get-rpm",        "sim-set-app-pas-rpm",        app_pas_get_rpm,        sim_set_app_pas_rpm,        app_pas_rpm) \
  X("setup-current",          "sim-set-setup-current",      setup_current,          sim_set_setup_current,      setup_current) \
  X("setup-current-in",       "sim-set-setup-current-in",   setup_current_in,       sim_set_setup_current_in,   setup_current_in) \
  X("setup-ah",               "sim-set-setup-ah",           setup_ah,               sim_set_setup_ah,           setup_ah) \
  X("setup-ah-chg",           "sim-set-setup-ah-chg",       setup_ah_chg,           sim_set_setup_ah_chg,       setup_ah_chg) \
  X("setup-wh",               "sim-set-setup-wh",           setup_wh,               sim_set_setup_wh,           setup_wh) \
  X("setup-wh-chg",           "sim-set-setup-wh-chg",       setup_wh_chg,           sim_set_setup_wh_chg,       setup_wh_chg) \
  X("gnss-height",            "sim-set-gnss-height",        gnss_height,            sim_set_gnss_height,        gnss_height) \
  X("gnss-speed",             "sim-set-gnss-speed",         gnss_speed,             sim_set_gnss_speed,         gnss_speed) \
  X("gnss-hdop",              "sim-set-gnss-hdop",          gnss_hdop,              sim_set_gnss_hdop,          gnss_hdop) \
  X("gnss-age",               "sim-set-gnss-age",           gnss_age,               sim_set_gnss_age,           gnss_age) \
  X("bme280-temp",            "sim-set-bme280-temp",        bme280_temp,            sim_set_bme280_temp,        bme280_temp) \
  X("bme280-hum",             "sim-set-bme280-hum",         bme280_hum,             sim_set_bme280_hum,         bme280_hum) \
  X("bme280-pres",            "sim-set-bme280-pres",        bme280_pres,            sim_set_bme280_pres,        bme280_pres) \
  X("phase-encoder",          "sim-set-phase-encoder",      phase_encoder,          sim_set_phase_encoder,      phase_encoder) \
  X("phase-hall",             "sim-set-phase-hall",         phase_hall,             sim_set_phase_hall,         phase_hall) \
  X("phase-motor",            "sim-set-phase-motor",        phase_motor,            sim_set_phase_motor,        phase_motor) \
  X("phase-observer",         "sim-set-phase-observer",     phase_observer,         sim_set_phase_observer,     phase_observer) \
  X("raw-mod-alpha",          "sim-set-mod-alpha",          raw_mod_alpha,          sim_set_mod_alpha,          mod_alpha) \
  X("raw-mod-beta",           "sim-set-mod-beta",           raw_mod_beta,           sim_set_mod_beta,           mod_beta) \
  X("raw-mod-alpha-measured", "sim-set-mod-alpha-measured", raw_mod_alpha_measured, sim_set_mod_alpha_measured, mod_alpha_measured) \
  X("raw-mod-beta-measured",  "sim-set-mod-beta-measured",  raw_mod_beta_measured,  sim_set_mod_beta_measured,  mod_beta_measured)

#define X(ln_get, ln_set, cn_get, cn_set, field) \
  static lbm_value ext_##cn_get(lbm_value *args, lbm_uint argn) { \
    (void)args; (void)argn; \
    lbm_mutex_lock(&mock_mutex); \
    float _v = mock_state.field; \
    lbm_mutex_unlock(&mock_mutex); \
    return lbm_enc_float(_v); \
  } \
  static lbm_value ext_##cn_set(lbm_value *args, lbm_uint argn) { \
    if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR; \
    lbm_mutex_lock(&mock_mutex); \
    mock_state.field = lbm_dec_as_float(args[0]); \
    lbm_mutex_unlock(&mock_mutex); \
    return ENC_SYM_TRUE; \
  }
SENSOR_FLOAT_LIST
#undef X

// ---------------------------------------------------------------------------
// SENSOR_BOOL: boolean sensor
//   DUT reads:       (ext-name)      -> ENC_SYM_TRUE or ENC_SYM_NIL
//   Testbench sets:  (sim-set-X v)   -> writes mock_state.field
//
// Table columns: (lbm_get_name, lbm_sim_name, c_getter, c_setter, field)
// ---------------------------------------------------------------------------

#define SENSOR_BOOL_LIST \
  X("connected-ble",         "sim-set-connected-ble",         connected_ble,         sim_set_connected_ble,         connected_ble) \
  X("connected-hub",         "sim-set-connected-hub",         connected_hub,         sim_set_connected_hub,         connected_hub) \
  X("connected-usb",         "sim-set-connected-usb",         connected_usb,         sim_set_connected_usb,         connected_usb) \
  X("connected-wifi",        "sim-set-connected-wifi",        connected_wifi,        sim_set_connected_wifi,        connected_wifi) \
  X("encoder-index-found",   "sim-set-encoder-index-found",   encoder_index_found,   sim_set_encoder_index_found,   encoder_index_found) \
  X("app-is-output-disabled","sim-set-app-is-output-disabled",app_is_output_disabled,sim_set_app_is_output_disabled,app_is_output_disabled) \
  X("app-adc-range-ok",      "sim-set-app-adc-range-ok",      app_adc_range_ok,      sim_set_app_adc_range_ok,      app_adc_range_ok)

#define X(ln_get, ln_set, cn_get, cn_set, field) \
  static lbm_value ext_##cn_get(lbm_value *args, lbm_uint argn) { \
    (void)args; (void)argn; \
    lbm_mutex_lock(&mock_mutex); \
    bool _v = mock_state.field; \
    lbm_mutex_unlock(&mock_mutex); \
    return _v ? ENC_SYM_TRUE : ENC_SYM_NIL; \
  } \
  static lbm_value ext_##cn_set(lbm_value *args, lbm_uint argn) { \
    if (argn < 1) return ENC_SYM_TERROR; \
    lbm_mutex_lock(&mock_mutex); \
    mock_state.field = lbm_is_symbol_nil(args[0]) ? false : true; \
    lbm_mutex_unlock(&mock_mutex); \
    return ENC_SYM_TRUE; \
  }
SENSOR_BOOL_LIST
#undef X

// ---------------------------------------------------------------------------
// SENSOR_INT: integer sensor
//
// Table columns: (lbm_get_name, lbm_sim_name, c_getter, c_setter, field)
// ---------------------------------------------------------------------------

#define SENSOR_INT_LIST \
  X("get-selected-motor", "sim-set-selected-motor", get_selected_motor, sim_set_selected_motor, selected_motor) \
  X("setup-num-vescs",    "sim-set-setup-num-vescs", setup_num_vescs,   sim_set_setup_num_vescs, setup_num_vescs)

#define X(ln_get, ln_set, cn_get, cn_set, field) \
  static lbm_value ext_##cn_get(lbm_value *args, lbm_uint argn) { \
    (void)args; (void)argn; \
    lbm_mutex_lock(&mock_mutex); \
    int32_t _v = mock_state.field; \
    lbm_mutex_unlock(&mock_mutex); \
    return lbm_enc_i32(_v); \
  } \
  static lbm_value ext_##cn_set(lbm_value *args, lbm_uint argn) { \
    if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR; \
    lbm_mutex_lock(&mock_mutex); \
    mock_state.field = lbm_dec_as_i32(args[0]); \
    lbm_mutex_unlock(&mock_mutex); \
    return ENC_SYM_TRUE; \
  }
SENSOR_INT_LIST
#undef X

// ---------------------------------------------------------------------------
// SENSOR_FLOAT3: 3-element float list sensor (IMU vectors)
//   DUT reads:       (get-X) -> (f0 f1 f2)
//   Testbench sets:  (sim-set-X f0 f1 f2)
//
// Table columns: (lbm_get_name, lbm_sim_name, c_getter, c_setter, array_field)
// ---------------------------------------------------------------------------

#define SENSOR_FLOAT3_LIST \
  X("get-imu-acc",       "sim-set-imu-acc",       get_imu_acc,       sim_set_imu_acc,       imu_acc) \
  X("get-imu-acc-derot", "sim-set-imu-acc-derot", get_imu_acc_derot, sim_set_imu_acc_derot, imu_acc_derot) \
  X("get-imu-gyro",      "sim-set-imu-gyro",      get_imu_gyro,      sim_set_imu_gyro,      imu_gyro) \
  X("get-imu-gyro-derot","sim-set-imu-gyro-derot",get_imu_gyro_derot,sim_set_imu_gyro_derot,imu_gyro_derot) \
  X("get-imu-mag",       "sim-set-imu-mag",       get_imu_mag,       sim_set_imu_mag,       imu_mag) \
  X("get-imu-rpy",       "sim-set-imu-rpy",       get_imu_rpy,       sim_set_imu_rpy,       imu_rpy)

#define X(ln_get, ln_set, cn_get, cn_set, arr) \
  static lbm_value ext_##cn_get(lbm_value *args, lbm_uint argn) { \
    (void)args; (void)argn; \
    lbm_mutex_lock(&mock_mutex); \
    float _x = mock_state.arr[0]; \
    float _y = mock_state.arr[1]; \
    float _z = mock_state.arr[2]; \
    lbm_mutex_unlock(&mock_mutex); \
    lbm_value _r = lbm_cons(lbm_enc_float(_z), ENC_SYM_NIL); \
    _r = lbm_cons(lbm_enc_float(_y), _r); \
    _r = lbm_cons(lbm_enc_float(_x), _r); \
    return _r; \
  } \
  static lbm_value ext_##cn_set(lbm_value *args, lbm_uint argn) { \
    if (argn < 3 || !lbm_is_number(args[0]) || \
        !lbm_is_number(args[1]) || !lbm_is_number(args[2])) return ENC_SYM_TERROR; \
    lbm_mutex_lock(&mock_mutex); \
    mock_state.arr[0] = lbm_dec_as_float(args[0]); \
    mock_state.arr[1] = lbm_dec_as_float(args[1]); \
    mock_state.arr[2] = lbm_dec_as_float(args[2]); \
    lbm_mutex_unlock(&mock_mutex); \
    return ENC_SYM_TRUE; \
  }
SENSOR_FLOAT3_LIST
#undef X

// ---------------------------------------------------------------------------
// SENSOR_FLOAT4: 4-element float list sensor (quaternion)
// ---------------------------------------------------------------------------

#define SENSOR_FLOAT4_LIST \
  X("get-imu-quat", "sim-set-imu-quat", get_imu_quat, sim_set_imu_quat, imu_quat)

#define X(ln_get, ln_set, cn_get, cn_set, arr) \
  static lbm_value ext_##cn_get(lbm_value *args, lbm_uint argn) { \
    (void)args; (void)argn; \
    lbm_mutex_lock(&mock_mutex); \
    float _a = mock_state.arr[0]; \
    float _b = mock_state.arr[1]; \
    float _c = mock_state.arr[2]; \
    float _d = mock_state.arr[3]; \
    lbm_mutex_unlock(&mock_mutex); \
    lbm_value _r = lbm_cons(lbm_enc_float(_d), ENC_SYM_NIL); \
    _r = lbm_cons(lbm_enc_float(_c), _r); \
    _r = lbm_cons(lbm_enc_float(_b), _r); \
    _r = lbm_cons(lbm_enc_float(_a), _r); \
    return _r; \
  } \
  static lbm_value ext_##cn_set(lbm_value *args, lbm_uint argn) { \
    if (argn < 4 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) || \
        !lbm_is_number(args[2]) || !lbm_is_number(args[3])) return ENC_SYM_TERROR; \
    lbm_mutex_lock(&mock_mutex); \
    mock_state.arr[0] = lbm_dec_as_float(args[0]); \
    mock_state.arr[1] = lbm_dec_as_float(args[1]); \
    mock_state.arr[2] = lbm_dec_as_float(args[2]); \
    mock_state.arr[3] = lbm_dec_as_float(args[3]); \
    lbm_mutex_unlock(&mock_mutex); \
    return ENC_SYM_TRUE; \
  }
SENSOR_FLOAT4_LIST
#undef X

// ---------------------------------------------------------------------------
// CMD_FLOAT: float command
//   DUT sets:        (set-X v)        -> writes mock_state.cmd_field
//   Testbench reads: (sim-get-X)      -> lbm_enc_float(mock_state.cmd_field)
//
// Table columns: (lbm_set_name, lbm_sim_name, c_setter, c_sim, cmd_field)
// ---------------------------------------------------------------------------

#define CMD_FLOAT_LIST \
  X("set-current",      "sim-get-current",       set_current,      sim_get_current,       cmd_current) \
  X("set-current-rel",  "sim-get-current-rel",   set_current_rel,  sim_get_current_rel,   cmd_current_rel) \
  X("set-duty",         "sim-get-duty",          set_duty,         sim_get_duty,          cmd_duty) \
  X("set-rpm",          "sim-get-rpm",           set_rpm,          sim_get_rpm,           cmd_rpm) \
  X("set-pos",          "sim-get-pos",           set_pos,          sim_get_pos,           cmd_pos) \
  X("set-brake",        "sim-get-brake",         set_brake,        sim_get_brake,         cmd_brake) \
  X("set-brake-rel",    "sim-get-brake-rel",     set_brake_rel,    sim_get_brake_rel,     cmd_brake_rel) \
  X("set-handbrake",    "sim-get-handbrake",     set_handbrake,    sim_get_handbrake,     cmd_handbrake) \
  X("set-handbrake-rel","sim-get-handbrake-rel", set_handbrake_rel,sim_get_handbrake_rel, cmd_handbrake_rel) \
  X("set-servo",        "sim-get-servo",         set_servo,        sim_get_servo,         cmd_servo) \
  X("set-encoder",      "sim-get-encoder",       set_encoder,      sim_get_encoder,       cmd_encoder)

#define X(ln_set, ln_sim, cn_set, cn_sim, field) \
  static lbm_value ext_##cn_set(lbm_value *args, lbm_uint argn) { \
    if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR; \
    lbm_mutex_lock(&mock_mutex); \
    mock_state.field = lbm_dec_as_float(args[0]); \
    lbm_mutex_unlock(&mock_mutex); \
    return ENC_SYM_TRUE; \
  } \
  static lbm_value ext_##cn_sim(lbm_value *args, lbm_uint argn) { \
    (void)args; (void)argn; \
    lbm_mutex_lock(&mock_mutex); \
    float _v = mock_state.field; \
    lbm_mutex_unlock(&mock_mutex); \
    return lbm_enc_float(_v); \
  }
CMD_FLOAT_LIST
#undef X

// ---------------------------------------------------------------------------
// Custom implementations
// ---------------------------------------------------------------------------

// gnss-lat-lon: returns (lat lon) as a 2-float list
static lbm_value ext_gnss_lat_lon(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  lbm_mutex_lock(&mock_mutex);
  float _lat = mock_state.gnss_lat;
  float _lon = mock_state.gnss_lon;
  lbm_mutex_unlock(&mock_mutex);
  lbm_value _r = lbm_cons(lbm_enc_float(_lon), ENC_SYM_NIL);
  _r = lbm_cons(lbm_enc_float(_lat), _r);
  return _r;
}

static lbm_value ext_sim_set_gnss_lat_lon(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  mock_state.gnss_lat = lbm_dec_as_float(args[0]);
  mock_state.gnss_lon = lbm_dec_as_float(args[1]);
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

// phase-all: returns (phase-encoder phase-hall phase-motor phase-observer)
static lbm_value ext_phase_all(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  lbm_mutex_lock(&mock_mutex);
  float _pe = mock_state.phase_encoder;
  float _ph = mock_state.phase_hall;
  float _pm = mock_state.phase_motor;
  float _po = mock_state.phase_observer;
  lbm_mutex_unlock(&mock_mutex);
  lbm_value _r = lbm_cons(lbm_enc_float(_po), ENC_SYM_NIL);
  _r = lbm_cons(lbm_enc_float(_pm), _r);
  _r = lbm_cons(lbm_enc_float(_ph), _r);
  _r = lbm_cons(lbm_enc_float(_pe), _r);
  return _r;
}

// get-fault: returns the fault code as a symbol
static lbm_value ext_get_fault(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  lbm_mutex_lock(&mock_mutex);
  char _fname[32];
  strncpy(_fname, mock_state.fault_name, sizeof(_fname) - 1);
  _fname[sizeof(_fname) - 1] = '\0';
  lbm_mutex_unlock(&mock_mutex);
  lbm_uint _sid;
  if (!lbm_get_symbol_by_name(_fname, &_sid)) {
    if (!lbm_add_symbol(_fname, &_sid)) return ENC_SYM_NIL;
  }
  return lbm_enc_sym(_sid);
}

static lbm_value ext_sim_set_fault(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_symbol(args[0])) return ENC_SYM_TERROR;
  const char *_name = lbm_get_name_by_symbol(lbm_dec_sym(args[0]));
  if (!_name) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  strncpy(mock_state.fault_name, _name, sizeof(mock_state.fault_name) - 1);
  mock_state.fault_name[sizeof(mock_state.fault_name) - 1] = '\0';
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

// rand / rand-max
static lbm_value ext_rand(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_i32(rand());
}

static lbm_value ext_rand_max(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_i32(RAND_MAX);
}

// get-adc / get-adc-decoded: optional channel arg (0-3)
static lbm_value ext_get_adc(lbm_value *args, lbm_uint argn) {
  int32_t _ch = (argn > 0 && lbm_is_number(args[0])) ? lbm_dec_as_i32(args[0]) : 0;
  if (_ch < 0 || _ch >= 4) _ch = 0;
  lbm_mutex_lock(&mock_mutex);
  float _v = mock_state.adc[(uint32_t)_ch];
  lbm_mutex_unlock(&mock_mutex);
  return lbm_enc_float(_v);
}

static lbm_value ext_get_adc_decoded(lbm_value *args, lbm_uint argn) {
  int32_t _ch = (argn > 0 && lbm_is_number(args[0])) ? lbm_dec_as_i32(args[0]) : 0;
  if (_ch < 0 || _ch >= 4) _ch = 0;
  lbm_mutex_lock(&mock_mutex);
  float _v = mock_state.adc_decoded[(uint32_t)_ch];
  lbm_mutex_unlock(&mock_mutex);
  return lbm_enc_float(_v);
}

// sim-set-adc / sim-set-adc-decoded: (channel value)
static lbm_value ext_sim_set_adc(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  int32_t _ch = lbm_dec_as_i32(args[0]);
  if (_ch < 0 || _ch >= 4) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  mock_state.adc[(uint32_t)_ch] = lbm_dec_as_float(args[1]);
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

static lbm_value ext_sim_set_adc_decoded(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  int32_t _ch = lbm_dec_as_i32(args[0]);
  if (_ch < 0 || _ch >= 4) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  mock_state.adc_decoded[(uint32_t)_ch] = lbm_dec_as_float(args[1]);
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

// sim-set-gpio / gpio-read: by integer pin index 0-15
static lbm_value ext_sim_set_gpio(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int32_t _pin = lbm_dec_as_i32(args[0]);
  if (_pin < 0 || _pin >= 16) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  mock_state.gpio[(uint32_t)_pin] = lbm_is_symbol_nil(args[1]) ? false : true;
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

static lbm_value ext_sim_get_gpio(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int32_t _pin = lbm_dec_as_i32(args[0]);
  if (_pin < 0 || _pin >= 16) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  bool _v = mock_state.gpio[(uint32_t)_pin];
  lbm_mutex_unlock(&mock_mutex);
  return _v ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

// select-motor: int command
static lbm_value ext_select_motor(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  mock_state.cmd_selected_motor = lbm_dec_as_i32(args[0]);
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

static lbm_value ext_sim_get_selected_motor(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  lbm_mutex_lock(&mock_mutex);
  int32_t _v = mock_state.cmd_selected_motor;
  lbm_mutex_unlock(&mock_mutex);
  return lbm_enc_i32(_v);
}

// set-kill-sw: bool command
static lbm_value ext_set_kill_sw(lbm_value *args, lbm_uint argn) {
  if (argn < 1) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  mock_state.cmd_kill_sw = lbm_is_symbol_nil(args[0]) ? false : true;
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

static lbm_value ext_sim_get_kill_sw(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  lbm_mutex_lock(&mock_mutex);
  bool _v = mock_state.cmd_kill_sw;
  lbm_mutex_unlock(&mock_mutex);
  return _v ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

// set-pos-time: variadic; store first arg as cmd_pos
static lbm_value ext_set_pos_time(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  lbm_mutex_lock(&mock_mutex);
  mock_state.cmd_pos = lbm_dec_as_float(args[0]);
  lbm_mutex_unlock(&mock_mutex);
  return ENC_SYM_TRUE;
}

// main-init-done: always true in the mock
static lbm_value ext_main_init_done(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_TRUE;
}

// puts: silently succeed (DUT print output is handled by the repl itself)
static lbm_value ext_puts(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_TRUE;
}

// set-print-prefix: silently succeed
static lbm_value ext_set_print_prefix(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_TRUE;
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

void load_vesc_extension_stubs(void) {
  lbm_mutex_init(&mock_mutex);
  memset(&mock_state, 0, sizeof(mock_state));
  strncpy(mock_state.fault_name, "no-fault", sizeof(mock_state.fault_name) - 1);
  mock_state.setup_num_vescs = 1;

  // Sensor float getters + sim-set-* setters
#define X(ln_get, ln_set, cn_get, cn_set, field) \
  lbm_add_extension(ln_get, ext_##cn_get); \
  lbm_add_extension(ln_set, ext_##cn_set);
  SENSOR_FLOAT_LIST
#undef X

  // foc-est-* are aliases to the same fields as get-est-*
  lbm_add_extension("foc-est-ind",    ext_get_est_ind);
  lbm_add_extension("foc-est-lambda", ext_get_est_lambda);
  lbm_add_extension("foc-est-res",    ext_get_est_res);
  lbm_add_extension("foc-hfi-res",    ext_get_hfi_res);

  // Sensor bool getters + sim-set-* setters
#define X(ln_get, ln_set, cn_get, cn_set, field) \
  lbm_add_extension(ln_get, ext_##cn_get); \
  lbm_add_extension(ln_set, ext_##cn_set);
  SENSOR_BOOL_LIST
#undef X

  // Sensor int getters + sim-set-* setters
#define X(ln_get, ln_set, cn_get, cn_set, field) \
  lbm_add_extension(ln_get, ext_##cn_get); \
  lbm_add_extension(ln_set, ext_##cn_set);
  SENSOR_INT_LIST
#undef X

  // Sensor float3 getters + sim-set-* setters
#define X(ln_get, ln_set, cn_get, cn_set, arr) \
  lbm_add_extension(ln_get, ext_##cn_get); \
  lbm_add_extension(ln_set, ext_##cn_set);
  SENSOR_FLOAT3_LIST
#undef X

  // Sensor float4 getter + sim-set-* setter
#define X(ln_get, ln_set, cn_get, cn_set, arr) \
  lbm_add_extension(ln_get, ext_##cn_get); \
  lbm_add_extension(ln_set, ext_##cn_set);
  SENSOR_FLOAT4_LIST
#undef X

  // Command float setters + sim-get-* readers
#define X(ln_set, ln_sim, cn_set, cn_sim, field) \
  lbm_add_extension(ln_set, ext_##cn_set); \
  lbm_add_extension(ln_sim, ext_##cn_sim);
  CMD_FLOAT_LIST
#undef X

  // Custom implementations
  lbm_add_extension("gnss-lat-lon",          ext_gnss_lat_lon);
  lbm_add_extension("sim-set-gnss-lat-lon",  ext_sim_set_gnss_lat_lon);
  lbm_add_extension("phase-all",             ext_phase_all);
  lbm_add_extension("get-fault",             ext_get_fault);
  lbm_add_extension("sim-set-fault",         ext_sim_set_fault);
  lbm_add_extension("rand",                  ext_rand);
  lbm_add_extension("rand-max",              ext_rand_max);
  lbm_add_extension("get-adc",               ext_get_adc);
  lbm_add_extension("get-adc-decoded",       ext_get_adc_decoded);
  lbm_add_extension("sim-set-adc",           ext_sim_set_adc);
  lbm_add_extension("sim-set-adc-decoded",   ext_sim_set_adc_decoded);
  lbm_add_extension("sim-set-gpio",          ext_sim_set_gpio);
  lbm_add_extension("sim-get-gpio",          ext_sim_get_gpio);
  lbm_add_extension("select-motor",          ext_select_motor);
  lbm_add_extension("sim-get-selected-motor",ext_sim_get_selected_motor);
  lbm_add_extension("set-kill-sw",           ext_set_kill_sw);
  lbm_add_extension("sim-get-kill-sw",       ext_sim_get_kill_sw);
  lbm_add_extension("set-pos-time",          ext_set_pos_time);
  lbm_add_extension("main-init-done",        ext_main_init_done);
  lbm_add_extension("puts",                  ext_puts);
  lbm_add_extension("set-print-prefix",      ext_set_print_prefix);

  // Nil stubs: hardware peripherals, firmware ops, complex/dynamic-list returns
  lbm_add_extension("aes-ctr-crypt",           ext_stub);
  lbm_add_extension("app-adc-detach",          ext_stub);
  lbm_add_extension("app-adc-override",        ext_stub);
  lbm_add_extension("app-disable-output",      ext_stub);

  lbm_add_extension("can-cmd",                 ext_stub);
  lbm_add_extension("can-fw-version",          ext_stub);
  lbm_add_extension("can-list-devs",           ext_stub);
  lbm_add_extension("can-local-id",            ext_stub);
  lbm_add_extension("can-msg-age",             ext_stub);
  lbm_add_extension("can-ping",                ext_stub);
  lbm_add_extension("can-recv-eid",            ext_stub);
  lbm_add_extension("can-recv-sid",            ext_stub);
  lbm_add_extension("can-scan",                ext_stub);
  lbm_add_extension("can-send-eid",            ext_stub);
  lbm_add_extension("can-send-sid",            ext_stub);
  lbm_add_extension("can-start",               ext_stub);
  lbm_add_extension("can-stop",                ext_stub);
  lbm_add_extension("can-update-baud",         ext_stub);
  lbm_add_extension("can-upload-relay",        ext_stub);
  lbm_add_extension("can-use-vesc",            ext_stub);
  lbm_add_extension("canget-adc",              ext_stub);
  lbm_add_extension("canget-current",          ext_stub);
  lbm_add_extension("canget-current-dir",      ext_stub);
  lbm_add_extension("canget-current-in",       ext_stub);
  lbm_add_extension("canget-dist",             ext_stub);
  lbm_add_extension("canget-duty",             ext_stub);
  lbm_add_extension("canget-ppm",              ext_stub);
  lbm_add_extension("canget-rpm",              ext_stub);
  lbm_add_extension("canget-speed",            ext_stub);
  lbm_add_extension("canget-status",           ext_stub);
  lbm_add_extension("canget-status-4",         ext_stub);
  lbm_add_extension("canget-status-5",         ext_stub);
  lbm_add_extension("canget-temp-fet",         ext_stub);
  lbm_add_extension("canget-temp-motor",       ext_stub);
  lbm_add_extension("canget-values",           ext_stub);
  lbm_add_extension("canget-vin",              ext_stub);
  lbm_add_extension("canmsg-recv",             ext_stub);
  lbm_add_extension("canmsg-send",             ext_stub);
  lbm_add_extension("canset-brake",            ext_stub);
  lbm_add_extension("canset-brake-rel",        ext_stub);
  lbm_add_extension("canset-current",          ext_stub);
  lbm_add_extension("canset-current-rel",      ext_stub);
  lbm_add_extension("canset-duty",             ext_stub);
  lbm_add_extension("canset-pos",              ext_stub);
  lbm_add_extension("canset-rpm",              ext_stub);
  lbm_add_extension("send-bms-can",            ext_stub);

  lbm_add_extension("app-ppm-detach",          ext_stub);
  lbm_add_extension("app-ppm-override",        ext_stub);
  lbm_add_extension("as504x-deinit",           ext_stub);
  lbm_add_extension("as504x-init",             ext_stub);
  // lbm_add_extension("bits-dec-int", ...);  // implemented in repl_exts.c
  // lbm_add_extension("bits-enc-int", ...);  // implemented in repl_exts.c
  lbm_add_extension("bme280-start",            ext_stub);
  lbm_add_extension("bme280-stop",             ext_stub);
  lbm_add_extension("bms-force-balance",       ext_stub);
  lbm_add_extension("bms-st",                  ext_stub);
  lbm_add_extension("bms-zero-offset",         ext_stub);
  lbm_add_extension("buf-resize",              ext_stub);
  lbm_add_extension("cmds-proc",               ext_stub);
  lbm_add_extension("cmds-start-stop",         ext_stub);
  lbm_add_extension("conf-dc-cal",             ext_stub);
  lbm_add_extension("conf-dc-cal-set",         ext_stub);
  lbm_add_extension("conf-detect-foc",         ext_stub);
  lbm_add_extension("conf-detect-hall",        ext_stub);
  lbm_add_extension("conf-detect-lambda-enc",  ext_stub);
  lbm_add_extension("conf-enc-sincos",         ext_stub);
  lbm_add_extension("conf-get",                ext_stub);
  lbm_add_extension("conf-get-limits",         ext_stub);
  lbm_add_extension("conf-measure-ind",        ext_stub);
  lbm_add_extension("conf-measure-res",        ext_stub);
  lbm_add_extension("conf-remap-as504x",       ext_stub);
  lbm_add_extension("conf-remap-hall",         ext_stub);
  lbm_add_extension("conf-restore-app",        ext_stub);
  lbm_add_extension("conf-restore-mc",         ext_stub);
  lbm_add_extension("conf-set",                ext_stub);
  lbm_add_extension("conf-set-pid-offset",     ext_stub);
  lbm_add_extension("conf-store",              ext_stub);
  lbm_add_extension("crc16",                   ext_stub);
  lbm_add_extension("crc32",                   ext_stub);
  lbm_add_extension("eeprom-erase",            ext_stub);
  lbm_add_extension("eeprom-read-f",           ext_stub);
  lbm_add_extension("eeprom-read-i",           ext_stub);
  lbm_add_extension("eeprom-store-f",          ext_stub);
  lbm_add_extension("eeprom-store-i",          ext_stub);
  lbm_add_extension("empty",                   ext_stub);
  lbm_add_extension("enc-corr",                ext_stub);
  lbm_add_extension("enc-corr-en",             ext_stub);
  lbm_add_extension("enc-sample",              ext_stub);
  lbm_add_extension("encoder-abi-state",       ext_stub);
  lbm_add_extension("esp-now-add-peer",        ext_stub);
  lbm_add_extension("esp-now-del-peer",        ext_stub);
  lbm_add_extension("esp-now-recv",            ext_stub);
  lbm_add_extension("esp-now-send",            ext_stub);
  lbm_add_extension("esp-now-start",           ext_stub);
  lbm_add_extension("event-enable",            ext_stub);
  lbm_add_extension("foc-beep",                ext_stub);
  lbm_add_extension("foc-openloop",            ext_stub);
  lbm_add_extension("foc-openloop-phase",      ext_stub);
  lbm_add_extension("foc-play-samples",        ext_stub);
  lbm_add_extension("foc-play-stop",           ext_stub);
  lbm_add_extension("foc-play-tone",           ext_stub);
  lbm_add_extension("foc-set-fw-override",     ext_stub);
  lbm_add_extension("fw-data",                 ext_stub);
  lbm_add_extension("fw-erase",                ext_stub);
  lbm_add_extension("fw-info",                 ext_stub);
  lbm_add_extension("fw-reboot",               ext_stub);
  lbm_add_extension("fw-write",                ext_stub);
  lbm_add_extension("fw-write-raw",            ext_stub);
  lbm_add_extension("get-bms-val",             ext_stub);
  lbm_add_extension("get-mac-addr",            ext_stub);
  lbm_add_extension("get-remote-state",        ext_stub);
  lbm_add_extension("gnss-date-time",          ext_stub);
  lbm_add_extension("gpio-configure",          ext_stub);
  lbm_add_extension("gpio-hold",               ext_stub);
  lbm_add_extension("gpio-hold-deepsleep",     ext_stub);
  lbm_add_extension("gpio-read",               ext_stub);
  lbm_add_extension("gpio-write",              ext_stub);
  lbm_add_extension("i2c-detect-addr",         ext_stub);
  lbm_add_extension("i2c-restore",             ext_stub);
  lbm_add_extension("i2c-start",               ext_stub);
  lbm_add_extension("i2c-tx-rx",               ext_stub);
  lbm_add_extension("icu-period",              ext_stub);
  lbm_add_extension("icu-start",               ext_stub);
  lbm_add_extension("icu-width",               ext_stub);
  // lbm_add_extension("image-save", ...);  // implemented in repl_exts.c
  lbm_add_extension("imu-start-lsm6",          ext_stub);
  lbm_add_extension("imu-stop",                ext_stub);
  lbm_add_extension("ioboard-get-adc",         ext_stub);
  lbm_add_extension("ioboard-get-digital",     ext_stub);
  lbm_add_extension("ioboard-set-digital",     ext_stub);
  lbm_add_extension("ioboard-set-pwm",         ext_stub);
  lbm_add_extension("lbm-erase",               ext_stub);
  lbm_add_extension("lbm-run",                 ext_stub);
  lbm_add_extension("lbm-set-gc-stack-size",   ext_stub);
  lbm_add_extension("lbm-set-quota",           ext_stub);
  lbm_add_extension("lbm-write",               ext_stub);
  lbm_add_extension("load-native-lib",         ext_stub);
  lbm_add_extension("log-config-field",        ext_stub);
  lbm_add_extension("log-send-f32",            ext_stub);
  lbm_add_extension("log-send-f64",            ext_stub);
  lbm_add_extension("log-start",               ext_stub);
  lbm_add_extension("log-stop",                ext_stub);
  lbm_add_extension("nmea-parse",              ext_stub);
  lbm_add_extension("nvs-erase",               ext_stub);
  lbm_add_extension("nvs-list",                ext_stub);
  lbm_add_extension("nvs-qml-erase-key",       ext_stub);
  lbm_add_extension("nvs-qml-erase-partition", ext_stub);
  lbm_add_extension("nvs-qml-init",            ext_stub);
  lbm_add_extension("nvs-qml-list",            ext_stub);
  lbm_add_extension("nvs-qml-read",            ext_stub);
  lbm_add_extension("nvs-qml-write",           ext_stub);
  lbm_add_extension("nvs-read",                ext_stub);
  lbm_add_extension("nvs-write",               ext_stub);

  lbm_add_extension("override-speed",          ext_stub);
  lbm_add_extension("override-temp-motor",     ext_stub);
  lbm_add_extension("plot-add-graph",          ext_stub);
  lbm_add_extension("plot-init",               ext_stub);
  lbm_add_extension("plot-send-points",        ext_stub);
  lbm_add_extension("plot-set-graph",          ext_stub);
  lbm_add_extension("prof-result",             ext_stub);
  lbm_add_extension("prof-trig",               ext_stub);
  lbm_add_extension("pwm-set-duty",            ext_stub);
  lbm_add_extension("pwm-start",               ext_stub);
  lbm_add_extension("pwm-stop",                ext_stub);
  lbm_add_extension("qml-erase",               ext_stub);
  lbm_add_extension("qml-write",               ext_stub);
  lbm_add_extension("raw-adc-current",         ext_stub);
  lbm_add_extension("raw-adc-voltage",         ext_stub);
  lbm_add_extension("raw-hall",                ext_stub);
  lbm_add_extension("reboot",                  ext_stub);
  lbm_add_extension("recv-data",               ext_stub);
  lbm_add_extension("reset-timeout",           ext_stub);
  lbm_add_extension("rtc-data",                ext_stub);
  // lbm_add_extension("secs-since", ...);  // implemented in repl_exts.c
  lbm_add_extension("send-data",               ext_stub);
  lbm_add_extension("set-aux",                 ext_stub);
  lbm_add_extension("set-bms-chg-allowed",     ext_stub);
  lbm_add_extension("set-bms-val",             ext_stub);
  lbm_add_extension("set-fw-name",             ext_stub);
  lbm_add_extension("set-odometer",            ext_stub);
  lbm_add_extension("set-remote-state",        ext_stub);
  lbm_add_extension("shutdown-hold",           ext_stub);
  lbm_add_extension("sleep-config-wakeup-pin", ext_stub);
  lbm_add_extension("sleep-deep",              ext_stub);
  lbm_add_extension("sleep-light",             ext_stub);
  lbm_add_extension("stats",                   ext_stub);
  lbm_add_extension("stats-reset",             ext_stub);
  lbm_add_extension("store-backup",            ext_stub);
  lbm_add_extension("sysinfo",                 ext_stub);
  // lbm_add_extension("systime", ...);  // implemented in repl_exts.c
  lbm_add_extension("throttle-curve",          ext_stub);
  lbm_add_extension("timeout-reset",           ext_stub);
  lbm_add_extension("uartcomm-start",          ext_stub);
  lbm_add_extension("uartcomm-stop",           ext_stub);
  lbm_add_extension("uart-read",               ext_stub);
  lbm_add_extension("uart-start",              ext_stub);
  lbm_add_extension("uart-stop",               ext_stub);
  lbm_add_extension("uart-write",              ext_stub);
  lbm_add_extension("uavcan-last-rawcmd",      ext_stub);
  lbm_add_extension("uavcan-last-rpmcmd",      ext_stub);
  lbm_add_extension("ublox-init",              ext_stub);
  lbm_add_extension("unload-native-lib",       ext_stub);
  lbm_add_extension("wifi-get-bw",             ext_stub);
  lbm_add_extension("wifi-get-chan",            ext_stub);
  lbm_add_extension("wifi-set-bw",             ext_stub);
  lbm_add_extension("wifi-set-chan",            ext_stub);
  lbm_add_extension("wifi-start",              ext_stub);
  lbm_add_extension("wifi-stop",               ext_stub);
  lbm_add_extension("zip-ls",                  ext_stub);
}
