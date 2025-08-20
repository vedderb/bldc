#include "lispbm.h"

// Function stubs:

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_print
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_print_prefix(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_print_prefix
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_puts(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_puts
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_servo(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_servo
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_reset_timeout(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_reset_timeout
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_ppm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_ppm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_ppm_age(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_ppm_age
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_vin(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_vin
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_select_motor(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_select_motor
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_selected_motor(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_selected_motor
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_bms_val(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_bms_val
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_bms_val(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_bms_val
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_send_bms_can(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_send_bms_can
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_bms_chg_allowed(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_bms_chg_allowed
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_bms_force_balance(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_bms_force_balance
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_bms_zero_offset(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_bms_zero_offset
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_adc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_adc
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_override_temp_motor(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_override_temp_motor
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_adc_decoded(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_adc_decoded
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_systime(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_systime
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_secs_since
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_aux(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_aux
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_rpy(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_imu_rpy
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_quat(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_imu_quat
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_acc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_imu_acc
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_gyro(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_imu_gyro
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_mag(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_imu_mag
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_acc_derot(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_imu_acc_derot
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_gyro_derot(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_imu_gyro_derot
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_send_data(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_send_data
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_recv_data(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_recv_data
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_remote_state(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_remote_state
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_eeprom_store_f(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_eeprom_store_f
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_eeprom_read_f(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_eeprom_read_f
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_eeprom_store_i(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_eeprom_store_i
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_eeprom_read_i(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_eeprom_read_i
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_sysinfo(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_sysinfo
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_odometer(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_odometer
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_stats(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_stats
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_stats_reset(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_stats_reset
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_cmd(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_cmd
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_local_id(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_local_id
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_update_baud(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_update_baud
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_adc_detach(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_adc_detach
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_adc_override(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_adc_override
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_adc_range_ok(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_adc_range_ok
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_ppm_detach(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_ppm_detach
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_ppm_override(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_ppm_override
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_remote_state(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_remote_state
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_disable_output(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_disable_output
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_is_output_disabled(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_is_output_disabled
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_app_pas_get_rpm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_app_pas_get_rpm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_current(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_current
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_current_rel(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_current_rel
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_duty(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_duty
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_brake(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_brake
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_brake_rel(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_brake_rel
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_handbrake(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_handbrake
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_handbrake_rel(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_handbrake_rel
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_rpm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_rpm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_pos(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_pos
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_openloop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_openloop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_openloop_phase(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_openloop_phase
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_kill_sw(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_kill_sw
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_beep(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_beep
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_play_tone(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_play_tone
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_play_samples(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_play_samples
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_play_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_play_stop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_current(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_current
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_current_dir(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_current_dir
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_current_in(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_current_in
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_id(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_id
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_iq(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_iq
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_id_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_id_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_iq_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_iq_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_vd(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_vd
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_vq(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_vq
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_est_lambda(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_est_lambda
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_est_res(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_est_res
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_est_ind(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_est_ind
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_foc_hfi_res(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_foc_hfi_res
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_duty(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_duty
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_rpm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_rpm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_rpm_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_rpm_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_rpm_fast(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_rpm_fast
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_rpm_faster(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_rpm_faster
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_pos(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_pos
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_temp_fet(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_temp_fet
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_temp_mot(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_temp_mot
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_speed(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_speed
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_speed_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_speed_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_dist(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_dist
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_dist_abs(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_dist_abs
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_batt(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_batt
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_fault(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_fault
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_ah(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_ah
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_wh(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_wh
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_ah_chg(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_ah_chg
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_wh_chg(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_wh_chg
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_setup_ah(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_setup_ah
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_setup_ah_chg(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_setup_ah_chg
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_setup_wh(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_setup_wh
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_setup_wh_chg(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_setup_wh_chg
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_setup_current(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_setup_current
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_setup_current_in(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_setup_current_in
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_setup_num_vescs(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_setup_num_vescs
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_encoder(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_encoder
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_encoder(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_encoder
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_encoder_error_rate(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_encoder_error_rate
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_pos_pid_now(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_pos_pid_now
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_pos_pid_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_pos_pid_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_pos_pid_error(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_pos_pid_error
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_phase_motor(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_phase_motor
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_phase_encoder(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_phase_encoder
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_phase_hall(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_phase_hall
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_phase_observer(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_phase_observer
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_observer_error(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_observer_error
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_msg_age(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_msg_age
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_current(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_current
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_current_rel(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_current_rel
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_duty(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_duty
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_brake(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_brake
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_brake_rel(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_brake_rel
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_rpm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_rpm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_pos(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_pos
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_current(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_current
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_current_dir(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_current_dir
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_current_in(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_current_in
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_duty(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_duty
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_rpm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_rpm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_temp_fet(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_temp_fet
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_temp_motor(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_temp_motor
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_speed(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_speed
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_dist(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_dist
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_ppm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_ppm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_adc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_adc
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_get_vin(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_get_vin
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_list_devs(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_list_devs
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_scan(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_scan
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_ping(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_ping
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_send_sid(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_send_sid
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_send_eid(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_send_eid
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_recv_sid(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_recv_sid
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_recv_eid(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_recv_eid
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_throttle_curve(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_throttle_curve
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_rand(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_rand
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_rand_max(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_rand_max
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_bits_enc_int(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_bits_enc_int
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_bits_dec_int(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_bits_dec_int
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_enable_event(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_enable_event
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_raw_adc_current(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_raw_adc_current
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_raw_adc_voltage(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_raw_adc_voltage
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_raw_mod_alpha(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_raw_mod_alpha
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_raw_mod_beta(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_raw_mod_beta
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_raw_mod_alpha_measured(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_raw_mod_alpha_measured
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_raw_mod_beta_measured(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_raw_mod_beta_measured
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_raw_hall(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_raw_hall
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_uart_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uart_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_uart_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uart_stop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_uart_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uart_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_uart_read(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uart_read
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_i2c_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_i2c_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_i2c_tx_rx(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_i2c_tx_rx
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_i2c_detect_addr(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_i2c_detect_addr
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_i2c_restore(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_i2c_restore
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gpio_configure(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gpio_configure
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gpio_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gpio_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gpio_read(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gpio_read
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_get(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_get
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_store(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_store
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_detect_foc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_detect_foc
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_set_pid_offset(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_set_pid_offset
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_measure_res(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_measure_res
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_measure_ind(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_measure_ind
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_restore_mc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_restore_mc
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_restore_app(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_restore_app
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_dc_cal(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_dc_cal
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_dc_cal_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_dc_cal_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_enc_sincos(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_enc_sincos
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_get_limits(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_get_limits
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_detect_lambda_enc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_detect_lambda_enc
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_uavcan_last_rawcmd(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uavcan_last_rawcmd
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_uavcan_last_rpmcmd(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uavcan_last_rpmcmd
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_lbm_set_quota(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_lbm_set_quota
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_lbm_set_gc_stack_size(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_lbm_set_gc_stack_size
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_plot_init(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_plot_init
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_plot_add_graph(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_plot_add_graph
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_plot_set_graph(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_plot_set_graph
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_plot_send_points(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_plot_send_points
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_ioboard_get_adc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_ioboard_get_adc
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_ioboard_get_digital(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_ioboard_get_digital
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_ioboard_set_digital(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_ioboard_set_digital
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_ioboard_set_pwm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_ioboard_set_pwm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_log_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_log_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_log_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_log_stop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_log_config_field(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_log_config_field
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_log_send_f32(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_log_send_f32
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_log_send_f64(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_log_send_f64
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gnss_lat_lon(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gnss_lat_lon
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gnss_height(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gnss_height
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gnss_speed(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gnss_speed
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gnss_hdop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gnss_hdop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gnss_date_time(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gnss_date_time
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gnss_age(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gnss_age
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_empty(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_empty
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_load_native_lib(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_load_native_lib
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_unload_native_lib(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_unload_native_lib
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_icu_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_icu_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_icu_width(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_icu_width
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_icu_period(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_icu_period
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_crc16(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_crc16
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_crc32(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_crc32
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_buf_resize(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_buf_resize
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_shutdown_hold(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_shutdown_hold
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_override_speed(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_override_speed
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_canmsg_recv(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_canmsg_recv
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_canmsg_send(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_canmsg_send
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_pwm_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_pwm_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_pwm_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_pwm_stop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_pwm_set_duty(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_pwm_set_duty
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_image_save(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_image_save
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_cmds_start_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_cmds_start_stop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_cmds_proc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_cmds_proc
    return lbm_enc_sym(SYM_EERROR);
}

// Extension registration function:

void load_bldc_extensions(void) {
    lbm_add_extension("print", ext_print);
    lbm_add_extension("set-print-prefix", ext_set_print_prefix);
    lbm_add_extension("puts", ext_puts);
    lbm_add_extension("set-servo", ext_set_servo);
    lbm_add_extension("reset-timeout", ext_reset_timeout);
    lbm_add_extension("get-ppm", ext_get_ppm);
    lbm_add_extension("get-ppm-age", ext_get_ppm_age);
    lbm_add_extension("get-vin", ext_get_vin);
    lbm_add_extension("select-motor", ext_select_motor);
    lbm_add_extension("get-selected-motor", ext_get_selected_motor);
    lbm_add_extension("get-bms-val", ext_get_bms_val);
    lbm_add_extension("set-bms-val", ext_set_bms_val);
    lbm_add_extension("send-bms-can", ext_send_bms_can);
    lbm_add_extension("set-bms-chg-allowed", ext_set_bms_chg_allowed);
    lbm_add_extension("bms-force-balance", ext_bms_force_balance);
    lbm_add_extension("bms-zero-offset", ext_bms_zero_offset);
    lbm_add_extension("get-adc", ext_get_adc);
    lbm_add_extension("override-temp-motor", ext_override_temp_motor);
    lbm_add_extension("get-adc-decoded", ext_get_adc_decoded);
    lbm_add_extension("systime", ext_systime);
    lbm_add_extension("secs-since", ext_secs_since);
    lbm_add_extension("set-aux", ext_set_aux);
    lbm_add_extension("get-imu-rpy", ext_get_imu_rpy);
    lbm_add_extension("get-imu-quat", ext_get_imu_quat);
    lbm_add_extension("get-imu-acc", ext_get_imu_acc);
    lbm_add_extension("get-imu-gyro", ext_get_imu_gyro);
    lbm_add_extension("get-imu-mag", ext_get_imu_mag);
    lbm_add_extension("get-imu-acc-derot", ext_get_imu_acc_derot);
    lbm_add_extension("get-imu-gyro-derot", ext_get_imu_gyro_derot);
    lbm_add_extension("send-data", ext_send_data);
    lbm_add_extension("recv-data", ext_recv_data);
    lbm_add_extension("get-remote-state", ext_get_remote_state);
    lbm_add_extension("eeprom-store-f", ext_eeprom_store_f);
    lbm_add_extension("eeprom-read-f", ext_eeprom_read_f);
    lbm_add_extension("eeprom-store-i", ext_eeprom_store_i);
    lbm_add_extension("eeprom-read-i", ext_eeprom_read_i);
    lbm_add_extension("sysinfo", ext_sysinfo);
    lbm_add_extension("set-odometer", ext_set_odometer);
    lbm_add_extension("stats", ext_stats);
    lbm_add_extension("stats-reset", ext_stats_reset);
    lbm_add_extension("can-cmd", ext_can_cmd);
    lbm_add_extension("can-local-id", ext_can_local_id);
    lbm_add_extension("can-update-baud", ext_can_update_baud);
    lbm_add_extension("app-adc-detach", ext_app_adc_detach);
    lbm_add_extension("app-adc-override", ext_app_adc_override);
    lbm_add_extension("app-adc-range-ok", ext_app_adc_range_ok);
    lbm_add_extension("app-ppm-detach", ext_app_ppm_detach);
    lbm_add_extension("app-ppm-override", ext_app_ppm_override);
    lbm_add_extension("set-remote-state", ext_set_remote_state);
    lbm_add_extension("app-disable-output", ext_app_disable_output);
    lbm_add_extension("app-is-output-disabled", ext_app_is_output_disabled);
    lbm_add_extension("app-pas-get-rpm", ext_app_pas_get_rpm);
    lbm_add_extension("set-current", ext_set_current);
    lbm_add_extension("set-current-rel", ext_set_current_rel);
    lbm_add_extension("set-duty", ext_set_duty);
    lbm_add_extension("set-brake", ext_set_brake);
    lbm_add_extension("set-brake-rel", ext_set_brake_rel);
    lbm_add_extension("set-handbrake", ext_set_handbrake);
    lbm_add_extension("set-handbrake-rel", ext_set_handbrake_rel);
    lbm_add_extension("set-rpm", ext_set_rpm);
    lbm_add_extension("set-pos", ext_set_pos);
    lbm_add_extension("foc-openloop", ext_foc_openloop);
    lbm_add_extension("foc-openloop-phase", ext_foc_openloop_phase);
    lbm_add_extension("set-kill-sw", ext_set_kill_sw);
    lbm_add_extension("foc-beep", ext_foc_beep);
    lbm_add_extension("foc-play-tone", ext_foc_play_tone);
    lbm_add_extension("foc-play-samples", ext_foc_play_samples);
    lbm_add_extension("foc-play-stop", ext_foc_play_stop);
    lbm_add_extension("get-current", ext_get_current);
    lbm_add_extension("get-current-dir", ext_get_current_dir);
    lbm_add_extension("get-current-in", ext_get_current_in);
    lbm_add_extension("get-id", ext_get_id);
    lbm_add_extension("get-iq", ext_get_iq);
    lbm_add_extension("get-id-set", ext_get_id_set);
    lbm_add_extension("get-iq-set", ext_get_iq_set);
    lbm_add_extension("get-vd", ext_get_vd);
    lbm_add_extension("get-vq", ext_get_vq);
    lbm_add_extension("foc-est-lambda", ext_foc_est_lambda);
    lbm_add_extension("foc-est-res", ext_foc_est_res);
    lbm_add_extension("foc-est-ind", ext_foc_est_ind);
    lbm_add_extension("foc-hfi-res", ext_foc_hfi_res);
    lbm_add_extension("get-duty", ext_get_duty);
    lbm_add_extension("get-rpm", ext_get_rpm);
    lbm_add_extension("get-rpm-set", ext_get_rpm_set);
    lbm_add_extension("get-rpm-fast", ext_get_rpm_fast);
    lbm_add_extension("get-rpm-faster", ext_get_rpm_faster);
    lbm_add_extension("get-pos", ext_get_pos);
    lbm_add_extension("get-temp-fet", ext_get_temp_fet);
    lbm_add_extension("get-temp-mot", ext_get_temp_mot);
    lbm_add_extension("get-speed", ext_get_speed);
    lbm_add_extension("get-speed-set", ext_get_speed_set);
    lbm_add_extension("get-dist", ext_get_dist);
    lbm_add_extension("get-dist-abs", ext_get_dist_abs);
    lbm_add_extension("get-batt", ext_get_batt);
    lbm_add_extension("get-fault", ext_get_fault);
    lbm_add_extension("get-ah", ext_get_ah);
    lbm_add_extension("get-wh", ext_get_wh);
    lbm_add_extension("get-ah-chg", ext_get_ah_chg);
    lbm_add_extension("get-wh-chg", ext_get_wh_chg);
    lbm_add_extension("setup-ah", ext_setup_ah);
    lbm_add_extension("setup-ah-chg", ext_setup_ah_chg);
    lbm_add_extension("setup-wh", ext_setup_wh);
    lbm_add_extension("setup-wh-chg", ext_setup_wh_chg);
    lbm_add_extension("setup-current", ext_setup_current);
    lbm_add_extension("setup-current-in", ext_setup_current_in);
    lbm_add_extension("setup-num-vescs", ext_setup_num_vescs);
    lbm_add_extension("get-encoder", ext_get_encoder);
    lbm_add_extension("set-encoder", ext_set_encoder);
    lbm_add_extension("get-encoder-error-rate", ext_get_encoder_error_rate);
    lbm_add_extension("pos-pid-now", ext_pos_pid_now);
    lbm_add_extension("pos-pid-set", ext_pos_pid_set);
    lbm_add_extension("pos-pid-error", ext_pos_pid_error);
    lbm_add_extension("phase-motor", ext_phase_motor);
    lbm_add_extension("phase-encoder", ext_phase_encoder);
    lbm_add_extension("phase-hall", ext_phase_hall);
    lbm_add_extension("phase-observer", ext_phase_observer);
    lbm_add_extension("observer-error", ext_observer_error);
    lbm_add_extension("can-msg-age", ext_can_msg_age);
    lbm_add_extension("can-current", ext_can_current);
    lbm_add_extension("can-current-rel", ext_can_current_rel);
    lbm_add_extension("can-duty", ext_can_duty);
    lbm_add_extension("can-brake", ext_can_brake);
    lbm_add_extension("can-brake-rel", ext_can_brake_rel);
    lbm_add_extension("can-rpm", ext_can_rpm);
    lbm_add_extension("can-pos", ext_can_pos);
    lbm_add_extension("can-get-current", ext_can_get_current);
    lbm_add_extension("can-get-current-dir", ext_can_get_current_dir);
    lbm_add_extension("can-get-current-in", ext_can_get_current_in);
    lbm_add_extension("can-get-duty", ext_can_get_duty);
    lbm_add_extension("can-get-rpm", ext_can_get_rpm);
    lbm_add_extension("can-get-temp-fet", ext_can_get_temp_fet);
    lbm_add_extension("can-get-temp-motor", ext_can_get_temp_motor);
    lbm_add_extension("can-get-speed", ext_can_get_speed);
    lbm_add_extension("can-get-dist", ext_can_get_dist);
    lbm_add_extension("can-get-ppm", ext_can_get_ppm);
    lbm_add_extension("can-get-adc", ext_can_get_adc);
    lbm_add_extension("can-get-vin", ext_can_get_vin);
    lbm_add_extension("can-list-devs", ext_can_list_devs);
    lbm_add_extension("can-scan", ext_can_scan);
    lbm_add_extension("can-ping", ext_can_ping);
    lbm_add_extension("can-send-sid", ext_can_send_sid);
    lbm_add_extension("can-send-eid", ext_can_send_eid);
    lbm_add_extension("can-recv-sid", ext_can_recv_sid);
    lbm_add_extension("can-recv-eid", ext_can_recv_eid);
    lbm_add_extension("throttle-curve", ext_throttle_curve);
    lbm_add_extension("rand", ext_rand);
    lbm_add_extension("rand-max", ext_rand_max);
    lbm_add_extension("bits-enc-int", ext_bits_enc_int);
    lbm_add_extension("bits-dec-int", ext_bits_dec_int);
    lbm_add_extension("enable-event", ext_enable_event);
    lbm_add_extension("raw-adc-current", ext_raw_adc_current);
    lbm_add_extension("raw-adc-voltage", ext_raw_adc_voltage);
    lbm_add_extension("raw-mod-alpha", ext_raw_mod_alpha);
    lbm_add_extension("raw-mod-beta", ext_raw_mod_beta);
    lbm_add_extension("raw-mod-alpha-measured", ext_raw_mod_alpha_measured);
    lbm_add_extension("raw-mod-beta-measured", ext_raw_mod_beta_measured);
    lbm_add_extension("raw-hall", ext_raw_hall);
    lbm_add_extension("uart-start", ext_uart_start);
    lbm_add_extension("uart-stop", ext_uart_stop);
    lbm_add_extension("uart-write", ext_uart_write);
    lbm_add_extension("uart-read", ext_uart_read);
    lbm_add_extension("i2c-start", ext_i2c_start);
    lbm_add_extension("i2c-tx-rx", ext_i2c_tx_rx);
    lbm_add_extension("i2c-detect-addr", ext_i2c_detect_addr);
    lbm_add_extension("i2c-restore", ext_i2c_restore);
    lbm_add_extension("gpio-configure", ext_gpio_configure);
    lbm_add_extension("gpio-write", ext_gpio_write);
    lbm_add_extension("gpio-read", ext_gpio_read);
    lbm_add_extension("conf-set", ext_conf_set);
    lbm_add_extension("conf-get", ext_conf_get);
    lbm_add_extension("conf-store", ext_conf_store);
    lbm_add_extension("conf-detect-foc", ext_conf_detect_foc);
    lbm_add_extension("conf-set-pid-offset", ext_conf_set_pid_offset);
    lbm_add_extension("conf-measure-res", ext_conf_measure_res);
    lbm_add_extension("conf-measure-ind", ext_conf_measure_ind);
    lbm_add_extension("conf-restore-mc", ext_conf_restore_mc);
    lbm_add_extension("conf-restore-app", ext_conf_restore_app);
    lbm_add_extension("conf-dc-cal", ext_conf_dc_cal);
    lbm_add_extension("conf-dc-cal-set", ext_conf_dc_cal_set);
    lbm_add_extension("conf-enc-sincos", ext_conf_enc_sincos);
    lbm_add_extension("conf-get-limits", ext_conf_get_limits);
    lbm_add_extension("conf-detect-lambda-enc", ext_conf_detect_lambda_enc);
    lbm_add_extension("uavcan-last-rawcmd", ext_uavcan_last_rawcmd);
    lbm_add_extension("uavcan-last-rpmcmd", ext_uavcan_last_rpmcmd);
    lbm_add_extension("lbm-set-quota", ext_lbm_set_quota);
    lbm_add_extension("lbm-set-gc-stack-size", ext_lbm_set_gc_stack_size);
    lbm_add_extension("plot-init", ext_plot_init);
    lbm_add_extension("plot-add-graph", ext_plot_add_graph);
    lbm_add_extension("plot-set-graph", ext_plot_set_graph);
    lbm_add_extension("plot-send-points", ext_plot_send_points);
    lbm_add_extension("ioboard-get-adc", ext_ioboard_get_adc);
    lbm_add_extension("ioboard-get-digital", ext_ioboard_get_digital);
    lbm_add_extension("ioboard-set-digital", ext_ioboard_set_digital);
    lbm_add_extension("ioboard-set-pwm", ext_ioboard_set_pwm);
    lbm_add_extension("log-start", ext_log_start);
    lbm_add_extension("log-stop", ext_log_stop);
    lbm_add_extension("log-config-field", ext_log_config_field);
    lbm_add_extension("log-send-f32", ext_log_send_f32);
    lbm_add_extension("log-send-f64", ext_log_send_f64);
    lbm_add_extension("gnss-lat-lon", ext_gnss_lat_lon);
    lbm_add_extension("gnss-height", ext_gnss_height);
    lbm_add_extension("gnss-speed", ext_gnss_speed);
    lbm_add_extension("gnss-hdop", ext_gnss_hdop);
    lbm_add_extension("gnss-date-time", ext_gnss_date_time);
    lbm_add_extension("gnss-age", ext_gnss_age);
    lbm_add_extension("empty", ext_empty);
    lbm_add_extension("load-native-lib", ext_load_native_lib);
    lbm_add_extension("unload-native-lib", ext_unload_native_lib);
    lbm_add_extension("icu-start", ext_icu_start);
    lbm_add_extension("icu-width", ext_icu_width);
    lbm_add_extension("icu-period", ext_icu_period);
    lbm_add_extension("crc16", ext_crc16);
    lbm_add_extension("crc32", ext_crc32);
    lbm_add_extension("buf-resize", ext_buf_resize);
    lbm_add_extension("shutdown-hold", ext_shutdown_hold);
    lbm_add_extension("override-speed", ext_override_speed);
    lbm_add_extension("canmsg-recv", ext_canmsg_recv);
    lbm_add_extension("canmsg-send", ext_canmsg_send);
    lbm_add_extension("pwm-start", ext_pwm_start);
    lbm_add_extension("pwm-stop", ext_pwm_stop);
    lbm_add_extension("pwm-set-duty", ext_pwm_set_duty);
    lbm_add_extension("image-save", ext_image_save);
    lbm_add_extension("cmds-start-stop", ext_cmds_start_stop);
    lbm_add_extension("cmds-proc", ext_cmds_proc);
}
