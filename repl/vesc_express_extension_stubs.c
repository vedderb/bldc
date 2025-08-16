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

static lbm_value ext_set_fw_name(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_fw_name
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_puts(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_puts
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

static lbm_value ext_conf_get(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_get
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_set(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_set
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_conf_store(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_conf_store
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_reboot(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_reboot
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_adc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_adc
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

static lbm_value ext_send_data(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_send_data
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_recv_data(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_recv_data
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

static lbm_value ext_eeprom_erase(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_eeprom_erase
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_sysinfo(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_sysinfo
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_cmd(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_cmd
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_msg_age(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_msg_age
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

static lbm_value ext_can_local_id(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_local_id
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_update_baud(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_update_baud
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_stop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_can_use_vesc(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_can_use_vesc
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

static lbm_value ext_esp_now_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_esp_now_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_esp_now_add_peer(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_esp_now_add_peer
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_esp_now_del_peer(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_esp_now_del_peer
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_mac_addr(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_get_mac_addr
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_wifi_set_chan(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_wifi_set_chan
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_wifi_get_chan(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_wifi_get_chan
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_wifi_set_bw(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_wifi_set_bw
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_wifi_get_bw(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_wifi_get_bw
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_wifi_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_wifi_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_wifi_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_wifi_stop
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_esp_now_send(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_esp_now_send
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_esp_now_recv(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_esp_now_recv
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

static lbm_value ext_gpio_configure(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gpio_configure
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gpio_hold(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gpio_hold
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_gpio_hold_deepsleep(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_gpio_hold_deepsleep
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

static lbm_value ext_main_init_done(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_main_init_done
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

static lbm_value ext_ublox_init(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_ublox_init
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nmea_parse(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nmea_parse
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_set_pos_time(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_set_pos_time
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_sleep_deep(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_sleep_deep
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_sleep_light(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_sleep_light
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_sleep_config_wakeup_pin(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_sleep_config_wakeup_pin
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_rtc_data(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_rtc_data
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_empty(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_empty
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

static lbm_value ext_f_connect(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_connect
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_connect_nand(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_connect_nand
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_disconnect(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_disconnect
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_open(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_open
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_close(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_close
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_read(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_read
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_readline(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_readline
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_tell(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_tell
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_seek(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_seek
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_mkdir(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_mkdir
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_rm(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_rm
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_ls(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_ls
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_size(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_size
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_rename(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_rename
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_sync(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_sync
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_f_fatinfo(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_f_fatinfo
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_fw_erase(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_fw_erase
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_fw_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_fw_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_fw_reboot(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_fw_reboot
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_lbm_erase(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_lbm_erase
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_qml_erase(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_qml_erase
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_lbm_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_lbm_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_qml_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_qml_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_fw_data(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_fw_data
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_fw_write_raw(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_fw_write_raw
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_fw_info(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_fw_info
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_lbm_run(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_lbm_run
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_bms_st(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_bms_st
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_as504x_init(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_as504x_init
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_as504x_deinit(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_as504x_deinit
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_as504x_angle(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_as504x_angle
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_imu_start_lsm6(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_imu_start_lsm6
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_imu_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_imu_stop
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

static lbm_value ext_uartcomm_start(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uartcomm_start
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_uartcomm_stop(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_uartcomm_stop
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

static lbm_value ext_unzip(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_unzip
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_zip_ls(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_zip_ls
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_connected_wifi(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_connected_wifi
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_connected_hub(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_connected_hub
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_connected_ble(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_connected_ble
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_connected_usb(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_connected_usb
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_aes_ctr_crypt(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_aes_ctr_crypt
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_qml_erase_partition(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_qml_erase_partition
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_qml_init(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_qml_init
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_read(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_read
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_qml_read(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_qml_read
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_qml_write(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_qml_write
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_qml_erase_key(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_qml_erase_key
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_erase(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_erase
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_image_save(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_image_save
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_qml_list(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_qml_list
    return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_nvs_list(lbm_value *args, lbm_uint argn) {
    // TODO: Implement ext_nvs_list
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

void load_vesc_express_extensions(void) {
    lbm_add_extension("print", ext_print);
    lbm_add_extension("set-print-prefix", ext_set_print_prefix);
    lbm_add_extension("set-fw-name", ext_set_fw_name);
    lbm_add_extension("puts", ext_puts);
    lbm_add_extension("get-bms-val", ext_get_bms_val);
    lbm_add_extension("set-bms-val", ext_set_bms_val);
    lbm_add_extension("send-bms-can", ext_send_bms_can);
    lbm_add_extension("set-bms-chg-allowed", ext_set_bms_chg_allowed);
    lbm_add_extension("bms-force-balance", ext_bms_force_balance);
    lbm_add_extension("bms-zero-offset", ext_bms_zero_offset);
    lbm_add_extension("conf-get", ext_conf_get);
    lbm_add_extension("conf-set", ext_conf_set);
    lbm_add_extension("conf-store", ext_conf_store);
    lbm_add_extension("reboot", ext_reboot);
    lbm_add_extension("get-adc", ext_get_adc);
    lbm_add_extension("systime", ext_systime);
    lbm_add_extension("secs-since", ext_secs_since);
    lbm_add_extension("send-data", ext_send_data);
    lbm_add_extension("recv-data", ext_recv_data);
    lbm_add_extension("eeprom-store-f", ext_eeprom_store_f);
    lbm_add_extension("eeprom-read-f", ext_eeprom_read_f);
    lbm_add_extension("eeprom-store-i", ext_eeprom_store_i);
    lbm_add_extension("eeprom-read-i", ext_eeprom_read_i);
    lbm_add_extension("eeprom-erase", ext_eeprom_erase);
    lbm_add_extension("sysinfo", ext_sysinfo);
    lbm_add_extension("can-cmd", ext_can_cmd);
    lbm_add_extension("can-msg-age", ext_can_msg_age);
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
    lbm_add_extension("can-local-id", ext_can_local_id);
    lbm_add_extension("can-update-baud", ext_can_update_baud);
    lbm_add_extension("can-start", ext_can_start);
    lbm_add_extension("can-stop", ext_can_stop);
    lbm_add_extension("can-use-vesc", ext_can_use_vesc);
    lbm_add_extension("can-scan", ext_can_scan);
    lbm_add_extension("can-ping", ext_can_ping);
    lbm_add_extension("can-send-sid", ext_can_send_sid);
    lbm_add_extension("can-send-eid", ext_can_send_eid);
    lbm_add_extension("can-recv-sid", ext_can_recv_sid);
    lbm_add_extension("can-recv-eid", ext_can_recv_eid);
    lbm_add_extension("can-current", ext_can_current);
    lbm_add_extension("can-current-rel", ext_can_current_rel);
    lbm_add_extension("can-duty", ext_can_duty);
    lbm_add_extension("can-brake", ext_can_brake);
    lbm_add_extension("can-brake-rel", ext_can_brake_rel);
    lbm_add_extension("can-rpm", ext_can_rpm);
    lbm_add_extension("can-pos", ext_can_pos);
    lbm_add_extension("throttle-curve", ext_throttle_curve);
    lbm_add_extension("rand", ext_rand);
    lbm_add_extension("rand-max", ext_rand_max);
    lbm_add_extension("bits-enc-int", ext_bits_enc_int);
    lbm_add_extension("bits-dec-int", ext_bits_dec_int);
    lbm_add_extension("enable-event", ext_enable_event);
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
    lbm_add_extension("esp-now-start", ext_esp_now_start);
    lbm_add_extension("esp-now-add-peer", ext_esp_now_add_peer);
    lbm_add_extension("esp-now-del-peer", ext_esp_now_del_peer);
    lbm_add_extension("get-mac-addr", ext_get_mac_addr);
    lbm_add_extension("wifi-set-chan", ext_wifi_set_chan);
    lbm_add_extension("wifi-get-chan", ext_wifi_get_chan);
    lbm_add_extension("wifi-set-bw", ext_wifi_set_bw);
    lbm_add_extension("wifi-get-bw", ext_wifi_get_bw);
    lbm_add_extension("wifi-start", ext_wifi_start);
    lbm_add_extension("wifi-stop", ext_wifi_stop);
    lbm_add_extension("esp-now-send", ext_esp_now_send);
    lbm_add_extension("esp-now-recv", ext_esp_now_recv);
    lbm_add_extension("i2c-start", ext_i2c_start);
    lbm_add_extension("i2c-tx-rx", ext_i2c_tx_rx);
    lbm_add_extension("i2c-detect-addr", ext_i2c_detect_addr);
    lbm_add_extension("gpio-configure", ext_gpio_configure);
    lbm_add_extension("gpio-hold", ext_gpio_hold);
    lbm_add_extension("gpio-hold-deepsleep", ext_gpio_hold_deepsleep);
    lbm_add_extension("gpio-write", ext_gpio_write);
    lbm_add_extension("gpio-read", ext_gpio_read);
    lbm_add_extension("main-init-done", ext_main_init_done);
    lbm_add_extension("crc16", ext_crc16);
    lbm_add_extension("crc32", ext_crc32);
    lbm_add_extension("buf-resize", ext_buf_resize);
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
    lbm_add_extension("ublox-init", ext_ublox_init);
    lbm_add_extension("nmea-parse", ext_nmea_parse);
    lbm_add_extension("set-pos-time", ext_set_pos_time);
    lbm_add_extension("sleep-deep", ext_sleep_deep);
    lbm_add_extension("sleep-light", ext_sleep_light);
    lbm_add_extension("sleep-config-wakeup-pin", ext_sleep_config_wakeup_pin);
    lbm_add_extension("rtc-data", ext_rtc_data);
    lbm_add_extension("empty", ext_empty);
    lbm_add_extension("canmsg-recv", ext_canmsg_recv);
    lbm_add_extension("canmsg-send", ext_canmsg_send);
    lbm_add_extension("f-connect", ext_f_connect);
    lbm_add_extension("f-connect-nand", ext_f_connect_nand);
    lbm_add_extension("f-disconnect", ext_f_disconnect);
    lbm_add_extension("f-open", ext_f_open);
    lbm_add_extension("f-close", ext_f_close);
    lbm_add_extension("f-read", ext_f_read);
    lbm_add_extension("f-readline", ext_f_readline);
    lbm_add_extension("f-write", ext_f_write);
    lbm_add_extension("f-tell", ext_f_tell);
    lbm_add_extension("f-seek", ext_f_seek);
    lbm_add_extension("f-mkdir", ext_f_mkdir);
    lbm_add_extension("f-rm", ext_f_rm);
    lbm_add_extension("f-ls", ext_f_ls);
    lbm_add_extension("f-size", ext_f_size);
    lbm_add_extension("f-rename", ext_f_rename);
    lbm_add_extension("f-sync", ext_f_sync);
    lbm_add_extension("f-fatinfo", ext_f_fatinfo);
    lbm_add_extension("fw-erase", ext_fw_erase);
    lbm_add_extension("fw-write", ext_fw_write);
    lbm_add_extension("fw-reboot", ext_fw_reboot);
    lbm_add_extension("lbm-erase", ext_lbm_erase);
    lbm_add_extension("qml-erase", ext_qml_erase);
    lbm_add_extension("lbm-write", ext_lbm_write);
    lbm_add_extension("qml-write", ext_qml_write);
    lbm_add_extension("fw-data", ext_fw_data);
    lbm_add_extension("fw-write-raw", ext_fw_write_raw);
    lbm_add_extension("fw-info", ext_fw_info);
    lbm_add_extension("lbm-run", ext_lbm_run);
    lbm_add_extension("bms-st", ext_bms_st);
    lbm_add_extension("as504x-init", ext_as504x_init);
    lbm_add_extension("as504x-deinit", ext_as504x_deinit);
    lbm_add_extension("as504x-angle", ext_as504x_angle);
    lbm_add_extension("imu-start-lsm6", ext_imu_start_lsm6);
    lbm_add_extension("imu-stop", ext_imu_stop);
    lbm_add_extension("get-imu-rpy", ext_get_imu_rpy);
    lbm_add_extension("get-imu-quat", ext_get_imu_quat);
    lbm_add_extension("get-imu-acc", ext_get_imu_acc);
    lbm_add_extension("get-imu-gyro", ext_get_imu_gyro);
    lbm_add_extension("get-imu-mag", ext_get_imu_mag);
    lbm_add_extension("get-imu-acc-derot", ext_get_imu_acc_derot);
    lbm_add_extension("get-imu-gyro-derot", ext_get_imu_gyro_derot);
    lbm_add_extension("uart-start", ext_uart_start);
    lbm_add_extension("uart-stop", ext_uart_stop);
    lbm_add_extension("uart-write", ext_uart_write);
    lbm_add_extension("uart-read", ext_uart_read);
    lbm_add_extension("uartcomm-start", ext_uartcomm_start);
    lbm_add_extension("uartcomm-stop", ext_uartcomm_stop);
    lbm_add_extension("pwm-start", ext_pwm_start);
    lbm_add_extension("pwm-stop", ext_pwm_stop);
    lbm_add_extension("pwm-set-duty", ext_pwm_set_duty);
    lbm_add_extension("unzip", ext_unzip);
    lbm_add_extension("zip-ls", ext_zip_ls);
    lbm_add_extension("connected-wifi", ext_connected_wifi);
    lbm_add_extension("connected-hub", ext_connected_hub);
    lbm_add_extension("connected-ble", ext_connected_ble);
    lbm_add_extension("connected-usb", ext_connected_usb);
    lbm_add_extension("aes-ctr-crypt", ext_aes_ctr_crypt);
    lbm_add_extension("nvs-qml-erase-partition", ext_nvs_qml_erase_partition);
    lbm_add_extension("nvs-qml-init", ext_nvs_qml_init);
    lbm_add_extension("nvs-read", ext_nvs_read);
    lbm_add_extension("nvs-qml-read", ext_nvs_qml_read);
    lbm_add_extension("nvs-write", ext_nvs_write);
    lbm_add_extension("nvs-qml-write", ext_nvs_qml_write);
    lbm_add_extension("nvs-qml-erase-key", ext_nvs_qml_erase_key);
    lbm_add_extension("nvs-erase", ext_nvs_erase);
    lbm_add_extension("image-save", ext_image_save);
    lbm_add_extension("nvs-qml-list", ext_nvs_qml_list);
    lbm_add_extension("nvs-list", ext_nvs_list);
    lbm_add_extension("cmds-start-stop", ext_cmds_start_stop);
    lbm_add_extension("cmds-proc", ext_cmds_proc);
}
