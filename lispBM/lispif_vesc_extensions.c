/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Joel Svensson    svenssonjoel@yahoo.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "lispif.h"
#include "lispbm.h"
#include "extensions/array_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/string_extensions.h"
#include "lbm_constants.h"

#include "commands.h"
#include "mc_interface.h"
#include "timeout.h"
#include "servo_dec.h"
#include "servo_simple.h"
#include "encoder/encoder.h"
#include "comm_can.h"
#include "bms.h"
#include "utils_math.h"
#include "utils_sys.h"
#include "hw.h"
#include "mcpwm_foc.h"
#include "imu.h"
#include "mempools.h"
#include "app.h"
#include "spi_bb.h"
#include "i2c.h"
#include "confgenerator.h"
#include "worker.h"
#include "app.h"
#include "canard_driver.h"
#include "firmware_metadata.h"
#include "log.h"
#include "buffer.h"
#include "crc.h"
#include "shutdown.h"

#include <math.h>
#include <ctype.h>
#include <stdarg.h>

/**
 * Bytes per word in the LBM memory.
 */
#define LBM_WORD_SIZE 4

typedef struct {
	// BMS
	lbm_uint v_tot;
	lbm_uint v_charge;
	lbm_uint i_in;
	lbm_uint i_in_ic;
	lbm_uint ah_cnt;
	lbm_uint wh_cnt;
	lbm_uint cell_num;
	lbm_uint v_cell;
	lbm_uint bal_state;
	lbm_uint temp_adc_num;
	lbm_uint temps_adc;
	lbm_uint temp_ic;
	lbm_uint temp_hum;
	lbm_uint hum;
	lbm_uint temp_max_cell;
	lbm_uint soc;
	lbm_uint soh;
	lbm_uint can_id;
	lbm_uint ah_cnt_chg_total;
	lbm_uint wh_cnt_chg_total;
	lbm_uint ah_cnt_dis_total;
	lbm_uint wh_cnt_dis_total;
	lbm_uint msg_age;

	// GPIO
	lbm_uint pin_mode_out;
	lbm_uint pin_mode_od;
	lbm_uint pin_mode_od_pu;
	lbm_uint pin_mode_od_pd;
	lbm_uint pin_mode_in;
	lbm_uint pin_mode_in_pu;
	lbm_uint pin_mode_in_pd;
	lbm_uint pin_mode_analog;
	lbm_uint pin_rx;
	lbm_uint pin_tx;
	lbm_uint pin_swdio;
	lbm_uint pin_swclk;
	lbm_uint pin_hall1;
	lbm_uint pin_hall2;
	lbm_uint pin_hall3;
	lbm_uint pin_adc1;
	lbm_uint pin_adc2;
	lbm_uint pin_ppm;
#ifdef PIN_HW_1
	lbm_uint pin_hw_1;
#endif
#ifdef PIN_HW_2
	lbm_uint pin_hw_2;
#endif	
	

	// Settings
	lbm_uint l_current_min;
	lbm_uint l_current_max;
	lbm_uint l_current_min_scale;
	lbm_uint l_current_max_scale;
	lbm_uint l_in_current_min;
	lbm_uint l_in_current_max;
	lbm_uint l_abs_current_max;
	lbm_uint l_min_erpm;
	lbm_uint l_max_erpm;
	lbm_uint l_erpm_start;
	lbm_uint l_min_vin;
	lbm_uint l_max_vin;
	lbm_uint l_min_duty;
	lbm_uint l_max_duty;
	lbm_uint l_watt_min;
	lbm_uint l_watt_max;
	lbm_uint l_battery_cut_start;
	lbm_uint l_battery_cut_end;
	lbm_uint l_temp_motor_start;
	lbm_uint l_temp_motor_end;
	lbm_uint l_temp_accel_dec;
	lbm_uint bms_limit_mode;
	lbm_uint motor_type;
	lbm_uint foc_sensor_mode;
	lbm_uint foc_current_kp;
	lbm_uint foc_current_ki;
	lbm_uint foc_f_zv;
	lbm_uint foc_motor_l;
	lbm_uint foc_motor_ld_lq_diff;
	lbm_uint foc_motor_r;
	lbm_uint foc_motor_flux_linkage;
	lbm_uint foc_observer_gain;
	lbm_uint foc_hfi_voltage_start;
	lbm_uint foc_hfi_voltage_run;
	lbm_uint foc_hfi_voltage_max;
	lbm_uint foc_sl_erpm;
	lbm_uint foc_sl_erpm_start;
	lbm_uint foc_hall_t0;
	lbm_uint foc_hall_t1;
	lbm_uint foc_hall_t2;
	lbm_uint foc_hall_t3;
	lbm_uint foc_hall_t4;
	lbm_uint foc_hall_t5;
	lbm_uint foc_hall_t6;
	lbm_uint foc_hall_t7;
	lbm_uint foc_sl_erpm_hfi;
	lbm_uint foc_openloop_rpm;
	lbm_uint foc_openloop_rpm_low;
	lbm_uint foc_sl_openloop_time_lock;
	lbm_uint foc_sl_openloop_time_ramp;
	lbm_uint foc_sl_openloop_time;
	lbm_uint foc_temp_comp;
	lbm_uint foc_temp_comp_base_temp;
	lbm_uint foc_fw_current_max;
	lbm_uint foc_fw_duty_start;
	lbm_uint m_invert_direction;
	lbm_uint m_out_aux_mode;
	lbm_uint m_ntc_motor_beta;
	lbm_uint si_motor_poles;
	lbm_uint si_gear_ratio;
	lbm_uint si_wheel_diameter;
	lbm_uint si_battery_cells;
	lbm_uint si_battery_ah;
	lbm_uint min_speed;
	lbm_uint max_speed;
	lbm_uint controller_id;
	lbm_uint can_baud_rate;
	lbm_uint app_to_use;
	lbm_uint ppm_ctrl_type;
	lbm_uint ppm_pulse_start;
	lbm_uint ppm_pulse_end;
	lbm_uint ppm_pulse_center;
	lbm_uint ppm_ramp_time_pos;
	lbm_uint ppm_ramp_time_neg;
	lbm_uint adc_ctrl_type;
	lbm_uint adc_ramp_time_pos;
	lbm_uint adc_ramp_time_neg;
	lbm_uint adc_thr_hyst;
	lbm_uint adc_v1_start;
	lbm_uint adc_v1_end;
	lbm_uint adc_v1_min;
	lbm_uint adc_v1_max;
	lbm_uint pas_current_scaling;

	// Sysinfo
	lbm_uint hw_name;
	lbm_uint fw_ver;
	lbm_uint has_phase_filters;
	lbm_uint uuid;
	lbm_uint runtime;
	lbm_uint git_branch;
	lbm_uint git_hash;
	lbm_uint compiler;
	lbm_uint hw_type;

	// Statistics
	lbm_uint stat_speed_avg;
	lbm_uint stat_speed_max;
	lbm_uint stat_power_avg;
	lbm_uint stat_power_max;
	lbm_uint stat_current_avg;
	lbm_uint stat_current_max;
	lbm_uint stat_temp_mosfet_avg;
	lbm_uint stat_temp_mosfet_max;
	lbm_uint stat_temp_motor_avg;
	lbm_uint stat_temp_motor_max;
	lbm_uint stat_count_time;

	// Rates
	lbm_uint rate_100k;
	lbm_uint rate_200k;
	lbm_uint rate_400k;
	lbm_uint rate_700k;

	// Other
	lbm_uint half_duplex;
} vesc_syms;

static vesc_syms syms_vesc = {0};

static bool get_add_symbol(char *name, lbm_uint* id) {
	if (!lbm_get_symbol_by_name(name, id)) {
		if (!lbm_add_symbol_const(name, id)) {
			return false;
		}
	}

	return true;
}

static bool compare_symbol(lbm_uint sym, lbm_uint *comp) {
	if (*comp == 0) {
		if (comp == &syms_vesc.v_tot) {
			get_add_symbol("bms-v-tot", comp);
		} else if (comp == &syms_vesc.v_charge) {
			get_add_symbol("bms-v-charge", comp);
		} else if (comp == &syms_vesc.i_in) {
			get_add_symbol("bms-i-in", comp);
		} else if (comp == &syms_vesc.i_in_ic) {
			get_add_symbol("bms-i-in-ic", comp);
		} else if (comp == &syms_vesc.ah_cnt) {
			get_add_symbol("bms-ah-cnt", comp);
		} else if (comp == &syms_vesc.wh_cnt) {
			get_add_symbol("bms-wh-cnt", comp);
		} else if (comp == &syms_vesc.cell_num) {
			get_add_symbol("bms-cell-num", comp);
		} else if (comp == &syms_vesc.v_cell) {
			get_add_symbol("bms-v-cell", comp);
		} else if (comp == &syms_vesc.bal_state) {
			get_add_symbol("bms-bal-state", comp);
		} else if (comp == &syms_vesc.temp_adc_num) {
			get_add_symbol("bms-temp-adc-num", comp);
		} else if (comp == &syms_vesc.temps_adc) {
			get_add_symbol("bms-temps-adc", comp);
		} else if (comp == &syms_vesc.temp_ic) {
			get_add_symbol("bms-temp-ic", comp);
		} else if (comp == &syms_vesc.temp_hum) {
			get_add_symbol("bms-temp-hum", comp);
		} else if (comp == &syms_vesc.hum) {
			get_add_symbol("bms-hum", comp);
		} else if (comp == &syms_vesc.temp_max_cell) {
			get_add_symbol("bms-temp-cell-max", comp);
		} else if (comp == &syms_vesc.soc) {
			get_add_symbol("bms-soc", comp);
		} else if (comp == &syms_vesc.soh) {
			get_add_symbol("bms-soh", comp);
		} else if (comp == &syms_vesc.can_id) {
			get_add_symbol("bms-can-id", comp);
		} else if (comp == &syms_vesc.ah_cnt_chg_total) {
			get_add_symbol("bms-ah-cnt-chg-total", comp);
		} else if (comp == &syms_vesc.wh_cnt_chg_total) {
			get_add_symbol("bms-wh-cnt-chg-total", comp);
		} else if (comp == &syms_vesc.ah_cnt_dis_total) {
			get_add_symbol("bms-ah-cnt-dis-total", comp);
		} else if (comp == &syms_vesc.wh_cnt_dis_total) {
			get_add_symbol("bms-wh-cnt-dis-total", comp);
		} else if (comp == &syms_vesc.msg_age) {
			get_add_symbol("bms-msg-age", comp);
		}

		else if (comp == &syms_vesc.pin_mode_out) {
			get_add_symbol("pin-mode-out", comp);
		} else if (comp == &syms_vesc.pin_mode_od) {
			get_add_symbol("pin-mode-od", comp);
		} else if (comp == &syms_vesc.pin_mode_od_pu) {
			get_add_symbol("pin-mode-od-pu", comp);
		} else if (comp == &syms_vesc.pin_mode_od_pd) {
			get_add_symbol("pin-mode-od-pd", comp);
		} else if (comp == &syms_vesc.pin_mode_in) {
			get_add_symbol("pin-mode-in", comp);
		} else if (comp == &syms_vesc.pin_mode_in_pu) {
			get_add_symbol("pin-mode-in-pu", comp);
		} else if (comp == &syms_vesc.pin_mode_in_pd) {
			get_add_symbol("pin-mode-in-pd", comp);
		} else if (comp == &syms_vesc.pin_mode_analog) {
			get_add_symbol("pin-mode-analog", comp);
		} else if (comp == &syms_vesc.pin_rx) {
			get_add_symbol("pin-rx", comp);
		} else if (comp == &syms_vesc.pin_tx) {
			get_add_symbol("pin-tx", comp);
		} else if (comp == &syms_vesc.pin_swdio) {
			get_add_symbol("pin-swdio", comp);
		} else if (comp == &syms_vesc.pin_swclk) {
			get_add_symbol("pin-swclk", comp);
		} else if (comp == &syms_vesc.pin_hall1) {
			get_add_symbol("pin-hall1", comp);
		} else if (comp == &syms_vesc.pin_hall2) {
			get_add_symbol("pin-hall2", comp);
		} else if (comp == &syms_vesc.pin_hall3) {
			get_add_symbol("pin-hall3", comp);
		} else if (comp == &syms_vesc.pin_adc1) {
			get_add_symbol("pin-adc1", comp);
		} else if (comp == &syms_vesc.pin_adc2) {
			get_add_symbol("pin-adc2", comp);
		} else if (comp == &syms_vesc.pin_ppm) {
			get_add_symbol("pin-ppm", comp);
		} 
#ifdef HW_PIN_1
		else if (comp == &syms_vesc.hw_pin_1) {
			get_add_symbol("hwpin-1", comp);
		} 
#endif
#ifdef HW_PIN_2
		else if (comp == &syms_vesc.hw_pin_2) {
			get_add_symbol("hwpin-2", comp);
		} 
#endif	

		else if (comp == &syms_vesc.l_current_min) {
			get_add_symbol("l-current-min", comp);
		} else if (comp == &syms_vesc.l_current_max) {
			get_add_symbol("l-current-max", comp);
		} else if (comp == &syms_vesc.l_current_min_scale) {
			get_add_symbol("l-current-min-scale", comp);
		} else if (comp == &syms_vesc.l_current_max_scale) {
			get_add_symbol("l-current-max-scale", comp);
		} else if (comp == &syms_vesc.l_in_current_min) {
			get_add_symbol("l-in-current-min", comp);
		} else if (comp == &syms_vesc.l_in_current_max) {
			get_add_symbol("l-in-current-max", comp);
		} else if (comp == &syms_vesc.l_abs_current_max) {
			get_add_symbol("l-abs-current-max", comp);
		} else if (comp == &syms_vesc.l_min_erpm) {
			get_add_symbol("l-min-erpm", comp);
		} else if (comp == &syms_vesc.l_max_erpm) {
			get_add_symbol("l-max-erpm", comp);
		} else if (comp == &syms_vesc.l_erpm_start) {
			get_add_symbol("l-erpm-start", comp);
		} else if (comp == &syms_vesc.l_min_vin) {
			get_add_symbol("l-min-vin", comp);
		} else if (comp == &syms_vesc.l_max_vin) {
			get_add_symbol("l-max-vin", comp);
		} else if (comp == &syms_vesc.l_min_duty) {
			get_add_symbol("l-min-duty", comp);
		} else if (comp == &syms_vesc.l_max_duty) {
			get_add_symbol("l-max-duty", comp);
		} else if (comp == &syms_vesc.l_watt_min) {
			get_add_symbol("l-watt-min", comp);
		} else if (comp == &syms_vesc.l_watt_max) {
			get_add_symbol("l-watt-max", comp);
		} else if (comp == &syms_vesc.l_battery_cut_start) {
			get_add_symbol("l-battery-cut-start", comp);
		} else if (comp == &syms_vesc.l_battery_cut_end) {
			get_add_symbol("l-battery-cut-end", comp);
		} else if (comp == &syms_vesc.l_temp_motor_start) {
			get_add_symbol("l-temp-motor-start", comp);
		} else if (comp == &syms_vesc.l_temp_motor_end) {
			get_add_symbol("l-temp-motor-end", comp);
		} else if (comp == &syms_vesc.l_temp_accel_dec) {
			get_add_symbol("l-temp-accel-dec", comp);
		} else if (comp == &syms_vesc.bms_limit_mode) {
			get_add_symbol("bms-limit-mode", comp);
		} else if (comp == &syms_vesc.motor_type) {
			get_add_symbol("motor-type", comp);
		} else if (comp == &syms_vesc.foc_sensor_mode) {
			get_add_symbol("foc-sensor-mode", comp);
		} else if (comp == &syms_vesc.foc_current_kp) {
			get_add_symbol("foc-current-kp", comp);
		} else if (comp == &syms_vesc.foc_current_ki) {
			get_add_symbol("foc-current-ki", comp);
		} else if (comp == &syms_vesc.foc_f_zv) {
			get_add_symbol("foc-f-zv", comp);
		} else if (comp == &syms_vesc.foc_motor_l) {
			get_add_symbol("foc-motor-l", comp);
		} else if (comp == &syms_vesc.foc_motor_ld_lq_diff) {
			get_add_symbol("foc-motor-ld-lq-diff", comp);
		} else if (comp == &syms_vesc.foc_motor_r) {
			get_add_symbol("foc-motor-r", comp);
		} else if (comp == &syms_vesc.foc_motor_flux_linkage) {
			get_add_symbol("foc-motor-flux-linkage", comp);
		} else if (comp == &syms_vesc.foc_observer_gain) {
			get_add_symbol("foc-observer-gain", comp);
		} else if (comp == &syms_vesc.foc_hfi_voltage_start) {
			get_add_symbol("foc-hfi-voltage-start", comp);
		} else if (comp == &syms_vesc.foc_hfi_voltage_run) {
			get_add_symbol("foc-hfi-voltage-run", comp);
		} else if (comp == &syms_vesc.foc_hfi_voltage_max) {
			get_add_symbol("foc-hfi-voltage-max", comp);
		} else if (comp == &syms_vesc.foc_sl_erpm) {
			get_add_symbol("foc-sl-erpm", comp);
		} else if (comp == &syms_vesc.foc_sl_erpm_start) {
			get_add_symbol("foc-sl-erpm-start", comp);
		} else if (comp == &syms_vesc.foc_hall_t0) {
			get_add_symbol("foc-hall-t0", comp);
		} else if (comp == &syms_vesc.foc_hall_t1) {
			get_add_symbol("foc-hall-t1", comp);
		} else if (comp == &syms_vesc.foc_hall_t2) {
			get_add_symbol("foc-hall-t2", comp);
		} else if (comp == &syms_vesc.foc_hall_t3) {
			get_add_symbol("foc-hall-t3", comp);
		} else if (comp == &syms_vesc.foc_hall_t4) {
			get_add_symbol("foc-hall-t4", comp);
		} else if (comp == &syms_vesc.foc_hall_t5) {
			get_add_symbol("foc-hall-t5", comp);
		} else if (comp == &syms_vesc.foc_hall_t6) {
			get_add_symbol("foc-hall-t6", comp);
		} else if (comp == &syms_vesc.foc_hall_t7) {
			get_add_symbol("foc-hall-t7", comp);
		} else if (comp == &syms_vesc.foc_sl_erpm_hfi) {
			get_add_symbol("foc-sl-erpm-hfi", comp);
		} else if (comp == &syms_vesc.foc_openloop_rpm) {
			get_add_symbol("foc-openloop-rpm", comp);
		} else if (comp == &syms_vesc.foc_openloop_rpm_low) {
			get_add_symbol("foc-openloop-rpm-low", comp);
		} else if (comp == &syms_vesc.foc_sl_openloop_time_lock) {
			get_add_symbol("foc-sl-openloop-time-lock", comp);
		} else if (comp == &syms_vesc.foc_sl_openloop_time_ramp) {
			get_add_symbol("foc-sl-openloop-time-ramp", comp);
		} else if (comp == &syms_vesc.foc_sl_openloop_time) {
			get_add_symbol("foc-sl-openloop-time", comp);
		} else if (comp == &syms_vesc.foc_temp_comp) {
			get_add_symbol("foc-temp-comp", comp);
		} else if (comp == &syms_vesc.foc_temp_comp_base_temp) {
			get_add_symbol("foc-temp-comp-base-temp", comp);
		} else if (comp == &syms_vesc.foc_fw_current_max) {
			get_add_symbol("foc-fw-current-max", comp);
		} else if (comp == &syms_vesc.foc_fw_duty_start) {
			get_add_symbol("foc-fw-duty-start", comp);
		} else if (comp == &syms_vesc.m_invert_direction) {
			get_add_symbol("m-invert-direction", comp);
		} else if (comp == &syms_vesc.m_out_aux_mode) {
			get_add_symbol("m-out-aux-mode", comp);
		} else if (comp == &syms_vesc.m_ntc_motor_beta) {
			get_add_symbol("m-ntc-motor-beta", comp);
		} else if (comp == &syms_vesc.si_motor_poles) {
			get_add_symbol("si-motor-poles", comp);
		} else if (comp == &syms_vesc.si_gear_ratio) {
			get_add_symbol("si-gear-ratio", comp);
		} else if (comp == &syms_vesc.si_wheel_diameter) {
			get_add_symbol("si-wheel-diameter", comp);
		} else if (comp == &syms_vesc.si_battery_cells) {
			get_add_symbol("si-battery-cells", comp);
		} else if (comp == &syms_vesc.si_battery_ah) {
			get_add_symbol("si-battery-ah", comp);
		} else if (comp == &syms_vesc.min_speed) {
			get_add_symbol("min-speed", comp);
		} else if (comp == &syms_vesc.max_speed) {
			get_add_symbol("max-speed", comp);
		} else if (comp == &syms_vesc.controller_id) {
			get_add_symbol("controller-id", comp);
		} else if (comp == &syms_vesc.can_baud_rate) {
			get_add_symbol("can-baud-rate", comp);
		} else if (comp == &syms_vesc.app_to_use) {
			get_add_symbol("app-to-use", comp);
		} else if (comp == &syms_vesc.ppm_ctrl_type) {
			get_add_symbol("ppm-ctrl-type", comp);
		} else if (comp == &syms_vesc.ppm_pulse_start) {
			get_add_symbol("ppm-pulse-start", comp);
		} else if (comp == &syms_vesc.ppm_pulse_end) {
			get_add_symbol("ppm-pulse-end", comp);
		} else if (comp == &syms_vesc.ppm_pulse_center) {
			get_add_symbol("ppm-pulse-center", comp);
		} else if (comp == &syms_vesc.ppm_ramp_time_pos) {
			get_add_symbol("ppm-ramp-time-pos", comp);
		} else if (comp == &syms_vesc.ppm_ramp_time_neg) {
			get_add_symbol("ppm-ramp-time-neg", comp);
		} else if (comp == &syms_vesc.adc_ctrl_type) {
			get_add_symbol("adc-ctrl-type", comp);
		} else if (comp == &syms_vesc.adc_ramp_time_pos) {
			get_add_symbol("adc-ramp-time-pos", comp);
		} else if (comp == &syms_vesc.adc_ramp_time_neg) {
			get_add_symbol("adc-ramp-time-neg", comp);
		} else if (comp == &syms_vesc.adc_thr_hyst) {
			get_add_symbol("adc-thr-hyst", comp);
		} else if (comp == &syms_vesc.adc_v1_start) {
			get_add_symbol("adc-v1-start", comp);
		} else if (comp == &syms_vesc.adc_v1_end) {
			get_add_symbol("adc-v1-end", comp);
		} else if (comp == &syms_vesc.adc_v1_min) {
			get_add_symbol("adc-v1-min", comp);
		} else if (comp == &syms_vesc.adc_v1_max) {
			get_add_symbol("adc-v1-max", comp);
		} else if (comp == &syms_vesc.pas_current_scaling) {
			get_add_symbol("pas-current-scaling", comp);
		}

		else if (comp == &syms_vesc.hw_name) {
			get_add_symbol("hw-name", comp);
		} else if (comp == &syms_vesc.fw_ver) {
			get_add_symbol("fw-ver", comp);
		} else if (comp == &syms_vesc.has_phase_filters) {
			get_add_symbol("has-phase-filters", comp);
		} else if (comp == &syms_vesc.uuid) {
			get_add_symbol("uuid", comp);
		} else if (comp == &syms_vesc.runtime) {
			get_add_symbol("runtime", comp);
		} else if (comp == &syms_vesc.git_branch) {
			get_add_symbol("git-branch", comp);
		} else if (comp == &syms_vesc.git_hash) {
			get_add_symbol("git-hash", comp);
		} else if (comp == &syms_vesc.compiler) {
			get_add_symbol("compiler", comp);
		} else if (comp == &syms_vesc.hw_type) {
			get_add_symbol("hw-type", comp);
		}

		else if (comp == &syms_vesc.stat_speed_avg) {
			get_add_symbol("stat-speed-avg", comp);
		} else if (comp == &syms_vesc.stat_speed_max) {
			get_add_symbol("stat-speed-max", comp);
		} else if (comp == &syms_vesc.stat_power_avg) {
			get_add_symbol("stat-power-avg", comp);
		} else if (comp == &syms_vesc.stat_power_max) {
			get_add_symbol("stat-power-max", comp);
		} else if (comp == &syms_vesc.stat_current_avg) {
			get_add_symbol("stat-current-avg", comp);
		} else if (comp == &syms_vesc.stat_current_max) {
			get_add_symbol("stat-current-max", comp);
		} else if (comp == &syms_vesc.stat_temp_mosfet_avg) {
			get_add_symbol("stat-temp-mosfet-avg", comp);
		} else if (comp == &syms_vesc.stat_temp_mosfet_max) {
			get_add_symbol("stat-temp-mosfet-max", comp);
		} else if (comp == &syms_vesc.stat_temp_motor_avg) {
			get_add_symbol("stat-temp-motor-avg", comp);
		} else if (comp == &syms_vesc.stat_temp_motor_max) {
			get_add_symbol("stat-temp-motor-max", comp);
		} else if (comp == &syms_vesc.stat_count_time) {
			get_add_symbol("stat-count-time", comp);
		}

		else if (comp == &syms_vesc.rate_100k) {
			get_add_symbol("rate-100k", comp);
		} else if (comp == &syms_vesc.rate_200k) {
			get_add_symbol("rate-200k", comp);
		} else if (comp == &syms_vesc.rate_400k) {
			get_add_symbol("rate-400k", comp);
		} else if (comp == &syms_vesc.rate_700k) {
			get_add_symbol("rate-700k", comp);
		}

		else if (comp == &syms_vesc.half_duplex) {
			get_add_symbol("half-duplex", comp);
		}
	}

	return *comp == sym;
}

static bool is_symbol_true_false(lbm_value v) {
	bool res = lbm_is_symbol_true(v) || lbm_is_symbol_nil(v);
	if (!res) {
		lbm_set_error_reason("Argument must be t or nil (true or false)");
	}
	return res;
}

/**
 * Wrapper around lbm_memory_shrink that takes number of bytes instead of number
 * of words. Shrinks the size of an pointer allocated in LBM memory to the
 * smallest possible size while still having capacity for the specified amount
 * of bytes.
 * 
 * @param ptr Pointer to the allocated segment in LBM memory. Should have been
 * obtained through lbm_malloc or other similar way at some point.
 * @param size_bytes The new capacity of the allocation in bytes. Must be
 * smaller or equal to the previous capacity.
 * @return If the operation succeeded. The return value of lbm_memory_shrink is
 * directly passed through, that is: false is returned either if ptr didn't
 * point into the LBM memory/didn't point to the start of an allocated segment
 * or if the new size was larger than the previous (note that since this
 * function converts bytes to words, a larger size in bytes might not cause it
 * to fail, as the size in words could still be the same). Otherwise true is
 * returned. 
*/
static bool lbm_memory_shrink_bytes(void *array, lbm_uint size_bytes) {
	lbm_uint size_words = size_bytes / LBM_WORD_SIZE;
	if (size_bytes % LBM_WORD_SIZE != 0) {
		size_words += 1;
	}
	
	return lbm_memory_shrink((lbm_uint *)array, size_words) > 0;
}

// Various commands

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
	const int str_len = 256;
	char *print_val_buffer = lbm_malloc_reserve(str_len);
	if (!print_val_buffer) {
		return ENC_SYM_MERROR;
	}

	for (lbm_uint i = 0; i < argn; i ++) {
		lbm_print_value(print_val_buffer, str_len, args[i]);
		commands_printf_lisp("%s", print_val_buffer);
	}

	lbm_free(print_val_buffer);

	return ENC_SYM_TRUE;
}

/**
 * signature: (puts string)
 *
 * Print string without surrounding it with "quotes" first.
 *
 * @param string The string to print. Strings longer than 400 characters will be
 * trimmed.
*/
static lbm_value ext_puts(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN(1);

	if (!lbm_is_array_r(args[0])) {
		return ENC_SYM_TERROR;
	}

	const char *string = lbm_dec_str(args[0]);
	commands_printf_lisp("%s", string);

	return ENC_SYM_TRUE;
}

static lbm_value ext_set_servo(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	servo_simple_set_output(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_reset_timeout(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	timeout_reset();
	return ENC_SYM_TRUE;
}

static lbm_value ext_get_ppm(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(lispif_get_ppm());
}

static lbm_value ext_get_ppm_age(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float((float)servodec_get_time_since_update() / 1000.0);
}

static lbm_value ext_get_vin(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_input_voltage_filtered());
}

static lbm_value ext_select_motor(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	int i = lbm_dec_as_i32(args[0]);
	if (i != 0 && i != 1 && i != 2) {
		return ENC_SYM_EERROR;
	}
	mc_interface_select_motor_thread(i);
	return ENC_SYM_TRUE;
}

static lbm_value ext_get_selected_motor(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(mc_interface_motor_now());
}

static lbm_value get_or_set_float(bool set, float *val, lbm_value *lbm_val) {
	if (set) {
		*val = lbm_dec_as_float(*lbm_val);
		return ENC_SYM_TRUE;
	} else {
		return lbm_enc_float(*val);
	}
}

static lbm_value get_or_set_i(bool set, int *val, lbm_value *lbm_val) {
	if (set) {
		*val = lbm_dec_as_i32(*lbm_val);
		return ENC_SYM_TRUE;
	} else {
		return lbm_enc_i(*val);
	}
}

static lbm_value get_or_set_bool(bool set, bool *val, lbm_value *lbm_val) {
	if (set) {
		*val = lbm_dec_as_i32(*lbm_val);
		return ENC_SYM_TRUE;
	} else {
		return lbm_enc_i(*val);
	}
}

static lbm_value get_set_bms_val(bool set, lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	lbm_value set_arg = 0;
	if (set && argn >= 1) {
		set_arg = args[argn - 1];
		argn--;

		if (!lbm_is_number(set_arg)) {
			lbm_set_error_reason((char*)lbm_error_str_no_number);
			return ENC_SYM_EERROR;
		}
	}

	if (argn != 1 && argn != 2) {
		return res;
	}

	if (lbm_type_of(args[0]) != LBM_TYPE_SYMBOL) {
		return res;
	}

	lbm_uint name = lbm_dec_sym(args[0]);
	bms_values *val = (bms_values*)bms_get_values();

	if (compare_symbol(name, &syms_vesc.v_tot)) {
		res = get_or_set_float(set, &val->v_tot, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.v_charge)) {
		res = get_or_set_float(set, &val->v_charge, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.i_in)) {
		res = get_or_set_float(set, &val->i_in, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.i_in_ic)) {
		res = get_or_set_float(set, &val->i_in_ic, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.ah_cnt)) {
		res = get_or_set_float(set, &val->ah_cnt, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.wh_cnt)) {
		res = get_or_set_float(set, &val->wh_cnt, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.cell_num)) {
		res = get_or_set_i(set, &val->cell_num, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.v_cell)) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return ENC_SYM_EERROR;
		}

		int c = lbm_dec_as_i32(args[1]);
		if (c < 0 || c >= val->cell_num) {
			return ENC_SYM_EERROR;
		}

		res = get_or_set_float(set, &val->v_cell[c], &set_arg);
	} else if (compare_symbol(name, &syms_vesc.bal_state)) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return ENC_SYM_EERROR;
		}

		int c = lbm_dec_as_i32(args[1]);
		if (c < 0 || c >= val->cell_num) {
			return ENC_SYM_EERROR;
		}

		res = get_or_set_bool(set, &val->bal_state[c], &set_arg);
	} else if (compare_symbol(name, &syms_vesc.temp_adc_num)) {
		res = get_or_set_i(set, &val->temp_adc_num, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.temps_adc)) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return ENC_SYM_EERROR;
		}

		int c = lbm_dec_as_i32(args[1]);
		if (c < 0 || c >= val->temp_adc_num) {
			return ENC_SYM_EERROR;
		}

		res = get_or_set_float(set, &val->temps_adc[c], &set_arg);
	} else if (compare_symbol(name, &syms_vesc.temp_ic)) {
		res = get_or_set_float(set, &val->temp_ic, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.temp_hum)) {
		res = get_or_set_float(set, &val->temp_hum, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.hum)) {
		res = get_or_set_float(set, &val->hum, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.temp_max_cell)) {
		res = get_or_set_float(set, &val->temp_max_cell, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.soc)) {
		res = get_or_set_float(set, &val->soc, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.soh)) {
		res = get_or_set_float(set, &val->soh, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.can_id)) {
		res = get_or_set_i(set, &val->can_id, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.ah_cnt_chg_total)) {
		res = get_or_set_float(set, &val->ah_cnt_chg_total, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.wh_cnt_chg_total)) {
		res = get_or_set_float(set, &val->wh_cnt_chg_total, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.ah_cnt_dis_total)) {
		res = get_or_set_float(set, &val->ah_cnt_dis_total, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.wh_cnt_dis_total)) {
		res = get_or_set_float(set, &val->wh_cnt_dis_total, &set_arg);
	} else if (compare_symbol(name, &syms_vesc.msg_age)) {
		res = lbm_enc_float(UTILS_AGE_S(val->update_time));
	}

	if (res != ENC_SYM_EERROR && set) {
		val->update_time = chVTGetSystemTimeX();
	}

	return res;
}

static lbm_value ext_get_bms_val(lbm_value *args, lbm_uint argn) {
	return get_set_bms_val(false, args, argn);
}

static lbm_value ext_set_bms_val(lbm_value *args, lbm_uint argn) {
	return get_set_bms_val(true, args, argn);
}

static lbm_value ext_send_bms_can(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	bms_send_status_can();
	return ENC_SYM_TRUE;
}

static lbm_value ext_get_adc(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn == 0) {
		return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT));
	} else if (argn == 1) {
		lbm_int channel = lbm_dec_as_i32(args[0]);
		if (channel == 0) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT));
		} else if (channel == 1) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT2));
		} else if (channel == 2) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT3));
		} else if (channel == 3) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_TEMP_MOTOR));
		} else if (channel == 4) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT4));
		} else if (channel == 5) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT5));
		} else if (channel == 6) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT6));
		} else if (channel == 7) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT7));
		} else if (channel == 8) {
			return lbm_enc_float(ADC_VOLTS(ADC_IND_EXT8));
		} else {
			return ENC_SYM_EERROR;
		}
	} else {
		return ENC_SYM_EERROR;
	}
}

static lbm_value ext_override_temp_motor(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	mc_interface_override_temp_motor(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_get_adc_decoded(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn == 0) {
		return lbm_enc_float(app_adc_get_decoded_level());
	} else if (argn == 1) {
		lbm_int channel = lbm_dec_as_i32(args[0]);
		if (channel == 0) {
			return lbm_enc_float(app_adc_get_decoded_level());
		} else if (channel == 1) {
			return lbm_enc_float(app_adc_get_decoded_level2());
		} else {
			return ENC_SYM_EERROR;
		}
	} else {
		return ENC_SYM_EERROR;
	}
}

static lbm_value ext_systime(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_u32(chVTGetSystemTimeX());
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	return lbm_enc_float(UTILS_AGE_S(lbm_dec_as_u32(args[0])));
}

static lbm_value ext_set_aux(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);

	int port = lbm_dec_as_u32(args[0]);
	bool on = lbm_dec_as_u32(args[1]);
	if (port == 1) {
		if (on) {
			AUX_ON();
		} else {
			AUX_OFF();
		}
		return ENC_SYM_TRUE;
	} else if (port == 2) {
		if (on) {
			AUX2_ON();
		} else {
			AUX2_OFF();
		}
		return ENC_SYM_TRUE;
	}

	return ENC_SYM_EERROR;
}

static lbm_value ext_get_imu_rpy(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float rpy[3];
	imu_get_rpy(rpy);

	lbm_value imu_data = ENC_SYM_NIL;
	imu_data = lbm_cons(lbm_enc_float(rpy[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(rpy[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(rpy[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_quat(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float q[4];
	imu_get_quaternions(q);

	lbm_value imu_data = ENC_SYM_NIL;
	imu_data = lbm_cons(lbm_enc_float(q[3]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(q[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(q[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(q[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_acc(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float acc[3];
	imu_get_accel(acc);

	lbm_value imu_data = ENC_SYM_NIL;
	imu_data = lbm_cons(lbm_enc_float(acc[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(acc[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(acc[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_gyro(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float gyro[3];
	imu_get_gyro(gyro);

	lbm_value imu_data = ENC_SYM_NIL;
	imu_data = lbm_cons(lbm_enc_float(gyro[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(gyro[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(gyro[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_mag(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float mag[3];
	imu_get_mag(mag);

	lbm_value imu_data = ENC_SYM_NIL;
	imu_data = lbm_cons(lbm_enc_float(mag[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(mag[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(mag[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_acc_derot(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float acc[3];
	imu_get_accel_derotated(acc);

	lbm_value imu_data = ENC_SYM_NIL;
	imu_data = lbm_cons(lbm_enc_float(acc[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(acc[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(acc[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_gyro_derot(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float gyro[3];
	imu_get_gyro_derotated(gyro);

	lbm_value imu_data = ENC_SYM_NIL;
	imu_data = lbm_cons(lbm_enc_float(gyro[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(gyro[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_float(gyro[0]), imu_data);

	return imu_data;
}

static lbm_value ext_send_data(lbm_value *args, lbm_uint argn) {
	if (argn != 1 || (!lbm_is_cons(args[0]) && !lbm_is_array_r(args[0]))) {
		return ENC_SYM_EERROR;
	}

	lbm_value curr = args[0];
	const int max_len = 50;
	uint8_t to_send[max_len];
	uint8_t *to_send_ptr = to_send;
	int ind = 0;

	if (lbm_is_array_r(args[0])) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
		to_send_ptr = (uint8_t*)array->data;
		ind = array->size;
	} else {
		while (lbm_is_cons(curr)) {
			lbm_value  arg = lbm_car(curr);

			if (lbm_is_number(arg)) {
				to_send[ind++] = lbm_dec_as_u32(arg);
			} else {
				return ENC_SYM_EERROR;
			}

			if (ind == max_len) {
				break;
			}

			curr = lbm_cdr(curr);
		}
	}

	commands_send_app_data(to_send_ptr, ind);

	return ENC_SYM_TRUE;
}

static volatile lbm_cid recv_data_cid = -1;

static lbm_value ext_recv_data(lbm_value *args, lbm_uint argn) {
	if (argn > 1 || (argn == 1 && !lbm_is_number(args[0]))) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
	}

	float timeout = -1.0;
	if (argn == 1) {
		timeout = lbm_dec_as_float(args[0]);
	}

	recv_data_cid = lbm_get_current_cid();

	if (timeout > 0.0) {
		lbm_block_ctx_from_extension_timeout(timeout);
	} else {
		lbm_block_ctx_from_extension();
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_get_remote_state(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float gyro[3];
	imu_get_gyro_derotated(gyro);

	lbm_value state = ENC_SYM_NIL;
	state = lbm_cons(lbm_enc_float(app_nunchuk_get_update_age()), state);
	state = lbm_cons(lbm_enc_i(app_nunchuk_get_is_rev()), state);
	state = lbm_cons(lbm_enc_i(app_nunchuk_get_bt_z()), state);
	state = lbm_cons(lbm_enc_i(app_nunchuk_get_bt_c()), state);
	state = lbm_cons(lbm_enc_float(app_nunchuk_get_decoded_x()), state);
	state = lbm_cons(lbm_enc_float(app_nunchuk_get_decoded_y()), state);

	return state;
}

static bool check_eeprom_addr(int addr) {
	if (addr < 0 || addr > 127) {
		lbm_set_error_reason("Address must be 0 to 127");
		return false;
	}

	return true;
}

static lbm_value ext_eeprom_store_f(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	v.as_float = lbm_dec_as_float(args[1]);
	return conf_general_store_eeprom_var_custom(&v, addr) ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_eeprom_read_f(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	bool res = conf_general_read_eeprom_var_custom(&v, addr);
	return res ? lbm_enc_float(v.as_float) : ENC_SYM_NIL;
}

static lbm_value ext_eeprom_store_i(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	v.as_i32 = lbm_dec_as_i32(args[1]);
	return conf_general_store_eeprom_var_custom(&v, addr) ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_eeprom_read_i(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	bool res = conf_general_read_eeprom_var_custom(&v, addr);
	return res ? lbm_enc_i32(v.as_i32) : ENC_SYM_NIL;
}

static lbm_uint sym_hw_esc;

static lbm_value ext_sysinfo(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if (argn != 1) {
		return res;
	}

	if (lbm_type_of(args[0]) != LBM_TYPE_SYMBOL) {
		return res;
	}

	lbm_uint name = lbm_dec_sym(args[0]);

	if (compare_symbol(name, &syms_vesc.hw_name)) {
		lbm_value lbm_res;
		if (lbm_create_array(&lbm_res, strlen(HW_NAME) + 1)) {
			lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
			strcpy((char*)arr->data, HW_NAME);
			res = lbm_res;
		} else {
			res = ENC_SYM_MERROR;
		}
	} else if (compare_symbol(name, &syms_vesc.fw_ver)) {
		res = ENC_SYM_NIL;
		res = lbm_cons(lbm_enc_i(FW_TEST_VERSION_NUMBER), res);
		res = lbm_cons(lbm_enc_i(FW_VERSION_MINOR), res);
		res = lbm_cons(lbm_enc_i(FW_VERSION_MAJOR), res);
	} else if (compare_symbol(name, &syms_vesc.has_phase_filters)) {
#ifdef HW_HAS_PHASE_FILTERS
		res = ENC_SYM_TRUE;
#else
		res = ENC_SYM_NIL;
#endif
	} else if (compare_symbol(name, &syms_vesc.uuid)) {
		res = ENC_SYM_NIL;
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[11]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[10]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[9]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[8]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[7]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[6]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[5]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[4]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[3]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[2]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[1]), res);
		res = lbm_cons(lbm_enc_i(STM32_UUID_8[0]), res);
	} else if (compare_symbol(name, &syms_vesc.runtime)) {
		res = lbm_enc_u64(g_backup.runtime);
	} else if (compare_symbol(name, &syms_vesc.git_branch)) {
		lbm_value lbm_res;
		if (lbm_create_array(&lbm_res, strlen(GIT_BRANCH_NAME) + 1)) {
			lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
			strcpy((char*)arr->data, GIT_BRANCH_NAME);
			res = lbm_res;
		} else {
			res = ENC_SYM_MERROR;
		}
	} else if (compare_symbol(name, &syms_vesc.git_hash)) {
		lbm_value lbm_res;
		if (lbm_create_array(&lbm_res, strlen(GIT_COMMIT_HASH) + 1)) {
			lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
			strcpy((char*)arr->data, GIT_COMMIT_HASH);
			res = lbm_res;
		} else {
			res = ENC_SYM_MERROR;
		}
	} else if (compare_symbol(name, &syms_vesc.compiler)) {
		lbm_value lbm_res;
		if (lbm_create_array(&lbm_res, strlen(ARM_GCC_VERSION) + 1)) {
			lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
			strcpy((char*)arr->data, ARM_GCC_VERSION);
			res = lbm_res;
		} else {
			res = ENC_SYM_MERROR;
		}
	} else if (compare_symbol(name, &syms_vesc.hw_type)) {
		res = lbm_enc_sym(sym_hw_esc);
	}

	return res;
}

static lbm_value ext_stats(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if (argn != 1) {
		return res;
	}

	if (lbm_type_of(args[0]) != LBM_TYPE_SYMBOL) {
		return res;
	}

	lbm_uint name = lbm_dec_sym(args[0]);

	if (compare_symbol(name, &syms_vesc.stat_speed_avg)) {
		res = lbm_enc_float(mc_interface_stat_speed_avg());
	} else if (compare_symbol(name, &syms_vesc.stat_speed_max)) {
		res = lbm_enc_float(mc_interface_stat_speed_max());
	} else if (compare_symbol(name, &syms_vesc.stat_power_avg)) {
		res = lbm_enc_float(mc_interface_stat_power_avg());
	} else if (compare_symbol(name, &syms_vesc.stat_power_max)) {
		res = lbm_enc_float(mc_interface_stat_power_max());
	} else if (compare_symbol(name, &syms_vesc.stat_current_avg)) {
		res = lbm_enc_float(mc_interface_stat_current_avg());
	} else if (compare_symbol(name, &syms_vesc.stat_current_max)) {
		res = lbm_enc_float(mc_interface_stat_current_max());
	} else if (compare_symbol(name, &syms_vesc.stat_temp_mosfet_avg)) {
		res = lbm_enc_float(mc_interface_stat_temp_mosfet_avg());
	} else if (compare_symbol(name, &syms_vesc.stat_temp_mosfet_max)) {
		res = lbm_enc_float(mc_interface_stat_temp_mosfet_max());
	} else if (compare_symbol(name, &syms_vesc.stat_temp_motor_avg)) {
		res = lbm_enc_float(mc_interface_stat_temp_motor_avg());
	} else if (compare_symbol(name, &syms_vesc.stat_temp_motor_max)) {
		res = lbm_enc_float(mc_interface_stat_temp_motor_max());
	} else if (compare_symbol(name, &syms_vesc.stat_count_time)) {
		res = lbm_enc_float(mc_interface_stat_count_time());
	}

	return res;
}

static lbm_value ext_stats_reset(lbm_value *args, lbm_uint argn) {
	(void)args;(void)argn;
	mc_interface_stat_reset();
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_cmd(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN(2);

	if (!lbm_is_number(args[0])) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
		return ENC_SYM_EERROR;
	}

	int id = lbm_dec_as_i32(args[0]);
	if (id < 0 || id > 255) {
		return ENC_SYM_EERROR;
	}

	if (!lbm_is_array_r(args[1])) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
		return ENC_SYM_EERROR;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);

	if (array->size > 500) {
		return ENC_SYM_EERROR;
	}

	uint8_t *send_buf = mempools_get_packet_buffer();
	send_buf[0] = COMM_LISP_REPL_CMD;
	memcpy(send_buf + 1, array->data, array->size);
	comm_can_send_buffer(id, send_buf, array->size + 1, 2);
	mempools_free_packet_buffer(send_buf);

	return ENC_SYM_TRUE;
}

static lbm_value ext_can_local_id(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(app_get_configuration()->controller_id);
}

// App set commands
static lbm_value ext_app_adc_detach(lbm_value *args, lbm_uint argn) {
	if (argn == 1) {
		if(lbm_dec_as_u32(args[0]) != 0) {
			return ENC_SYM_TERROR;
		}
	} else {
		LBM_CHECK_ARGN_NUMBER(2);
	}

	uint32_t mode = lbm_dec_as_u32(args[0]);
	int detach = lbm_dec_as_i32(args[1]);

	switch (mode){
		case 0:
			app_adc_detach_adc(0);
			app_adc_detach_buttons(false);
			break;
		case 1:
			app_adc_detach_adc(detach);
			app_adc_detach_buttons(false);
			break;
		case 2:
			app_adc_detach_adc(0);
			app_adc_detach_buttons(detach > 0);
			break;
		case 3:
			app_adc_detach_adc(detach);
			app_adc_detach_buttons(detach > 0);
			break;
		default:
			return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_app_adc_override(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);

	uint32_t target = lbm_dec_as_u32(args[0]);
	float val = lbm_dec_as_float(args[1]);
	bool state = val > 0.0 ? true : false;

	switch (target){
		case 0:
			app_adc_adc1_override(val);
			break;
		case 1:
			app_adc_adc2_override(val);
			break;
		case 2:
			app_adc_rev_override(state);
			break;
		case 3:
			app_adc_cc_override(state);
			break;
		default:
			return ENC_SYM_EERROR;
	}
	return ENC_SYM_TRUE;
}

static lbm_value ext_app_adc_range_ok(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return app_adc_range_ok() ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_app_ppm_detach(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	app_ppm_detach(lbm_dec_as_u32(args[0]) > 0);
	return ENC_SYM_TRUE;
}

static lbm_value ext_app_ppm_override(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	app_ppm_override(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_remote_state(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(5);
	chuck_data chuk = {0};

	float js_y = (lbm_dec_as_float(args[0]) + 1.0) * 128.0;
	utils_truncate_number(&js_y, 0.0, 255.0);
	chuk.js_y = (int)js_y;

	float js_x = (lbm_dec_as_float(args[1]) + 1.0) * 128.0;
	utils_truncate_number(&js_x, 0.0, 255.0);
	chuk.js_x = (int)js_x;

	chuk.bt_c = lbm_dec_as_u32(args[2]) > 0;
	chuk.bt_z = lbm_dec_as_u32(args[3]) > 0;
	chuk.is_rev = lbm_dec_as_u32(args[4]) > 0;
	chuk.rev_has_state = true;

	app_nunchuk_update_output(&chuk);

	return ENC_SYM_TRUE;
}

static lbm_value ext_app_disable_output(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	app_disable_output(lbm_dec_as_i32(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_app_is_output_disabled(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return app_is_output_disabled() ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_app_pas_get_rpm(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(app_pas_get_pedal_rpm());
}

// Motor set commands

static lbm_value ext_set_current(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();
	timeout_reset();

	if (argn == 1) {
		mc_interface_set_current(lbm_dec_as_float(args[0]));
	} else if (argn == 2) {
		mc_interface_set_current_off_delay(lbm_dec_as_float(args[1]));
		mc_interface_set_current(lbm_dec_as_float(args[0]));
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_set_current_rel(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();
	timeout_reset();

	if (argn == 1) {
		mc_interface_set_current_rel(lbm_dec_as_float(args[0]));
	} else if (argn == 2) {
		mc_interface_set_current_off_delay(lbm_dec_as_float(args[1]));
		mc_interface_set_current_rel(lbm_dec_as_float(args[0]));
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_set_duty(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_duty(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_brake(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_brake_current(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_brake_rel(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_brake_current_rel(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_handbrake(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_handbrake(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_handbrake_rel(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_handbrake_rel(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_rpm(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_pid_speed(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_pos(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_pid_pos(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_foc_openloop(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	timeout_reset();
	mc_interface_set_openloop_current(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_foc_beep(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(3);
	timeout_reset();
	mcpwm_foc_beep(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1]), lbm_dec_as_float(args[2]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_foc_play_tone(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(3);
	timeout_reset();
	bool res = mcpwm_foc_play_tone(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1]), lbm_dec_as_float(args[2]));
	return res ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

typedef struct {
	const int8_t *samples;
	int num_samp;
	float f_samp;
	float voltage;
	lbm_cid cid;
} foc_play_args;

static void foc_play_task(void *arg) {
	foc_play_args *fp = (foc_play_args*)arg;

	for (int i = 0;i < 10000;i++) {
		if (mcpwm_foc_play_audio_samples(fp->samples,
				fp->num_samp, fp->f_samp, fp->voltage)) {
			break;
		}
		chThdSleep(1);
	}

	lbm_unblock_ctx_unboxed(fp->cid, ENC_SYM_TRUE);
	lbm_free(fp);
}

static lbm_value ext_foc_play_samples(lbm_value *args, lbm_uint argn) {
	if (argn != 3 || !lbm_is_array_r(args[0]) || !lbm_is_number(args[1]) || !lbm_is_number(args[2])) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
		return ENC_SYM_TERROR;
	}

	timeout_reset();

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);

	const int8_t *samples = (const int8_t*)array->data;
	const int num_samp = array->size;
	const float f_samp = lbm_dec_as_float(args[1]);
	const float voltage = lbm_dec_as_float(args[2]);

	if (!mcpwm_foc_play_audio_samples(samples, num_samp, f_samp, voltage)) {
		foc_play_args *fp = lbm_malloc(sizeof(foc_play_args));
		fp->samples = samples;
		fp->num_samp = num_samp;
		fp->f_samp = f_samp;
		fp->voltage = voltage;
		fp->cid = lbm_get_current_cid();
		lbm_block_ctx_from_extension();
		worker_execute(foc_play_task, fp);
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_foc_play_stop(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	mcpwm_foc_stop_audio(true);
	return ENC_SYM_TRUE;
}

// Motor get commands

static bool check_arg_filter(lbm_value *args, lbm_uint argn, int *res) {
	*res = 0;
	if (argn > 1) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return false;
	}

	if (argn == 1) {
		if (!lbm_is_number(args[0])) {
			lbm_set_error_reason((char*)lbm_error_str_no_number);
			return false;
		}

		*res = lbm_dec_as_i32(args[0]);
	}

	return true;
}

static lbm_value ext_get_current(lbm_value *args, lbm_uint argn) {
	int filter = 0;
	if (!check_arg_filter(args, argn, &filter)) {
		return ENC_SYM_TERROR;
	}

	if (filter == 1) {
		return lbm_enc_float(mc_interface_read_reset_avg_motor_current());
	} else {
		return lbm_enc_float(mc_interface_get_tot_current_filtered());
	}
}

static lbm_value ext_get_current_dir(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_tot_current_directional_filtered());
}

static lbm_value ext_get_current_in(lbm_value *args, lbm_uint argn) {
	int filter = 0;
	if (!check_arg_filter(args, argn, &filter)) {
		return ENC_SYM_TERROR;
	}

	if (filter == 1) {
		return lbm_enc_float(mc_interface_read_reset_avg_input_current());
	} else {
		return lbm_enc_float(mc_interface_get_tot_current_in_filtered());
	}
}

static lbm_value ext_get_id(lbm_value *args, lbm_uint argn) {
	int filter = 0;
	if (!check_arg_filter(args, argn, &filter)) {
		return ENC_SYM_TERROR;
	}

	if (filter == 1) {
		return lbm_enc_float(mc_interface_read_reset_avg_id());
	} else {
		return lbm_enc_float(mcpwm_foc_get_id_filter());
	}
}

static lbm_value ext_get_iq(lbm_value *args, lbm_uint argn) {
	int filter = 0;
	if (!check_arg_filter(args, argn, &filter)) {
		return ENC_SYM_TERROR;
	}

	if (filter == 1) {
		return lbm_enc_float(mc_interface_read_reset_avg_iq());
	} else {
		return lbm_enc_float(mcpwm_foc_get_iq_filter());
	}
}

static lbm_value ext_get_id_set(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_id_set());
}

static lbm_value ext_get_iq_set(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_iq_set());
}

static lbm_value ext_get_vd(lbm_value *args, lbm_uint argn) {
	int filter = 0;
	if (!check_arg_filter(args, argn, &filter)) {
		return ENC_SYM_TERROR;
	}

	if (filter == 1) {
		return lbm_enc_float(mc_interface_read_reset_avg_vd());
	} else {
		return lbm_enc_float(mcpwm_foc_get_vd());
	}
}

static lbm_value ext_get_vq(lbm_value *args, lbm_uint argn) {
	int filter = 0;
	if (!check_arg_filter(args, argn, &filter)) {
		return ENC_SYM_TERROR;
	}

	if (filter == 1) {
		return lbm_enc_float(mc_interface_read_reset_avg_vq());
	} else {
		return lbm_enc_float(mcpwm_foc_get_vq());
	}
}

static lbm_value ext_foc_est_lambda(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_est_lambda());
}

static lbm_value ext_foc_est_res(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_est_res());
}

static lbm_value ext_foc_est_ind(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_est_ind());
}

static lbm_value ext_get_duty(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_duty_cycle_now());
}

static lbm_value ext_get_rpm(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_rpm());
}

static lbm_value ext_get_pos(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_pid_pos_now());
}

static lbm_value ext_get_temp_fet(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn > 1) {
		return ENC_SYM_EERROR;
	}

	int fet = 0;
	if (argn == 1) {
		fet = lbm_dec_as_i32(args[0]);
	}

	bool is_m2 = mc_interface_get_motor_thread() == 2;

	switch (fet) {
		case 0:
			return lbm_enc_float(mc_interface_temp_fet_filtered());
		case 1:
			return lbm_enc_float(is_m2 ? NTC_TEMP_MOS1_M2() : NTC_TEMP_MOS1());
		case 2:
			return lbm_enc_float(is_m2 ? NTC_TEMP_MOS2_M2() : NTC_TEMP_MOS2());
		case 3:
			return lbm_enc_float(is_m2 ? NTC_TEMP_MOS3_M2() : NTC_TEMP_MOS3());
		default:
			return ENC_SYM_EERROR;
	}
}

static lbm_value ext_get_temp_mot(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_temp_motor_filtered());
}

static lbm_value ext_get_speed(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_speed());
}

static lbm_value ext_get_dist(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_distance());
}

static lbm_value ext_get_dist_abs(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_distance_abs());
}

static lbm_value ext_get_batt(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_battery_level(0));
}

static lbm_value ext_get_fault(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(mc_interface_get_fault());
}

static lbm_value ext_get_ah(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_amp_hours(false));
}

static lbm_value ext_get_wh(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_watt_hours(false));
}

static lbm_value ext_get_ah_chg(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_amp_hours_charged(false));
}

static lbm_value ext_get_wh_chg(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_watt_hours_charged(false));
}

// Setup values

static lbm_value ext_setup_ah(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_setup_values().ah_tot);
}

static lbm_value ext_setup_ah_chg(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_setup_values().ah_charge_tot);
}

static lbm_value ext_setup_wh(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_setup_values().wh_tot);
}

static lbm_value ext_setup_wh_chg(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_setup_values().wh_charge_tot);
}

static lbm_value ext_setup_current(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_setup_values().current_tot);
}

static lbm_value ext_setup_current_in(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_setup_values().current_in_tot);
}

static lbm_value ext_setup_num_vescs(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(mc_interface_get_setup_values().num_vescs);
}

// Positions

static lbm_value ext_get_encoder(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(encoder_read_deg());
}

static lbm_value ext_set_encoder(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	encoder_set_deg(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_get_encoder_error_rate(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(encoder_get_error_rate());
}

static lbm_value ext_pos_pid_now(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_pid_pos_now());
}

static lbm_value ext_pos_pid_set(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_pid_pos_set());
}

static lbm_value ext_pos_pid_error(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(utils_angle_difference(mc_interface_get_pid_pos_set(), mc_interface_get_pid_pos_now()));
}

static lbm_value ext_phase_motor(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_phase());
}

static lbm_value ext_phase_encoder(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_phase_encoder());
}

static lbm_value ext_phase_hall(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_phase_hall());
}

static lbm_value ext_phase_observer(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_phase_observer());
}

static lbm_value ext_observer_error(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(utils_angle_difference(mcpwm_foc_get_phase_observer(), mcpwm_foc_get_phase_encoder()));
}

// CAN-commands

static lbm_value ext_can_current(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn == 2) {
		comm_can_set_current(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	} else if (argn == 3) {
		comm_can_set_current_off_delay(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]), lbm_dec_as_float(args[2]));
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_can_current_rel(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn == 2) {
		comm_can_set_current_rel(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	} else if (argn == 3) {
		comm_can_set_current_rel_off_delay(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]), lbm_dec_as_float(args[2]));
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_can_duty(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	comm_can_set_duty(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_brake(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	comm_can_set_current_brake(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_brake_rel(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	comm_can_set_current_brake_rel(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_rpm(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	comm_can_set_rpm(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_pos(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	comm_can_set_pos(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_get_current(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->current);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_current_dir(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->current * SIGN(stat0->duty));
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_current_in(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg_4 *stat4 = comm_can_get_status_msg_4_id(lbm_dec_as_i32(args[0]));
	if (stat4) {
		return lbm_enc_float((float)stat4->current_in);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_duty(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->duty);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_rpm(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->rpm);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_temp_fet(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg_4 *stat4 = comm_can_get_status_msg_4_id(lbm_dec_as_i32(args[0]));
	if (stat4) {
		return lbm_enc_float((float)stat4->temp_fet);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_temp_motor(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg_4 *stat4 = comm_can_get_status_msg_4_id(lbm_dec_as_i32(args[0]));
	if (stat4) {
		return lbm_enc_float((float)stat4->temp_motor);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_speed(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		const volatile mc_configuration *conf = mc_interface_get_configuration();
		const float rpm = stat0->rpm / (conf->si_motor_poles / 2.0);
		return lbm_enc_float((rpm / 60.0) * conf->si_wheel_diameter * M_PI / conf->si_gear_ratio);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_dist(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg_5 *stat5 = comm_can_get_status_msg_5_id(lbm_dec_as_i32(args[0]));
	if (stat5) {
		const volatile mc_configuration *conf = mc_interface_get_configuration();
		const float tacho_scale = (conf->si_wheel_diameter * M_PI) / (3.0 * conf->si_motor_poles * conf->si_gear_ratio);
		return lbm_enc_float((float)stat5->tacho_value * tacho_scale);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_ppm(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg_6 *stat6 = comm_can_get_status_msg_6_id(lbm_dec_as_i32(args[0]));
	if (stat6) {
		return lbm_enc_float((float)stat6->ppm);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_adc(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		return ENC_SYM_EERROR;
	}

	LBM_CHECK_NUMBER_ALL();

	lbm_int channel = 0;
	if (argn == 2) {
		channel = lbm_dec_as_i32(args[1]);
	}

	can_status_msg_6 *stat6 = comm_can_get_status_msg_6_id(lbm_dec_as_i32(args[0]));

	if (stat6) {
		if (channel == 0) {
			return lbm_enc_float(stat6->adc_1);
		} else if (channel == 1) {
			return lbm_enc_float(stat6->adc_2);
		} else if (channel == 2) {
			return lbm_enc_float(stat6->adc_3);
		} else {
			return ENC_SYM_EERROR;
		}
	} else {
		return lbm_enc_float(-1.0);
	}
}

static lbm_value ext_can_get_vin(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	can_status_msg_5 *stat5 = comm_can_get_status_msg_5_id(lbm_dec_as_i32(args[0]));
	if (stat5) {
		return lbm_enc_float(stat5->v_in);
	} else {
		return lbm_enc_float(0.0);
	}
}

static int cmp_int (const void * a, const void * b) {
	return ( *(int*)a - *(int*)b );
}

static lbm_value ext_can_list_devs(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	int dev_num = 0;
	can_status_msg *msg = comm_can_get_status_msg_index(dev_num);

	while (msg && msg->id >= 0) {
		dev_num++;
		msg = comm_can_get_status_msg_index(dev_num);
	}

	int devs[dev_num];

	for (int i = 0;i < dev_num;i++) {
		msg = comm_can_get_status_msg_index(i);
		if (msg) {
			devs[i] = msg->id;
		} else {
			devs[i] = -1;
		}
	}

	qsort(devs, dev_num, sizeof(int), cmp_int);
	lbm_value dev_list = ENC_SYM_NIL;

	for (int i = (dev_num - 1);i >= 0;i--) {
		if (devs[i] >= 0) {
			dev_list = lbm_cons(lbm_enc_i(devs[i]), dev_list);
		} else {
			break;
		}
	}

	return dev_list;
}

static lbm_value ext_can_scan(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	lbm_value dev_list = ENC_SYM_NIL;

	for (int i = 253;i >= 0;i--) {
		if (comm_can_ping(i, 0)) {
			dev_list = lbm_cons(lbm_enc_i(i), dev_list);
		}
	}

	return dev_list;
}

static lbm_value ext_can_send(lbm_value *args, lbm_uint argn, bool is_eid) {
	if (argn != 2 || !lbm_is_number(args[0])) {
		return ENC_SYM_EERROR;
	}

	lbm_value curr = args[1];
	uint8_t to_send[8];
	int ind = 0;

	if (lbm_is_array_r(curr)) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(curr);
		ind = array->size;
		if (ind > 8) {
			ind = 0;
		}

		memcpy(to_send, array->data, ind);
	} else {
		while (lbm_is_cons(curr)) {
			lbm_value  arg = lbm_car(curr);

			if (lbm_is_number(arg)) {
				to_send[ind++] = lbm_dec_as_u32(arg);
			} else {
				return ENC_SYM_EERROR;
			}

			if (ind == 8) {
				break;
			}

			curr = lbm_cdr(curr);
		}
	}

	if (is_eid) {
		comm_can_transmit_eid(lbm_dec_as_u32(args[0]), to_send, ind);
	} else {
		comm_can_transmit_sid(lbm_dec_as_u32(args[0]), to_send, ind);
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_can_send_sid(lbm_value *args, lbm_uint argn) {
	return ext_can_send(args, argn, false);
}

static lbm_value ext_can_send_eid(lbm_value *args, lbm_uint argn) {
	return ext_can_send(args, argn, true);
}

static volatile lbm_cid can_recv_sid_cid = -1;
static volatile lbm_cid can_recv_eid_cid = -1;

static lbm_value ext_can_recv_sid(lbm_value *args, lbm_uint argn) {
	if (argn > 1 || (argn == 1 && !lbm_is_number(args[0]))) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
	}

	float timeout = -1.0;
	if (argn == 1) {
		timeout = lbm_dec_as_float(args[0]);
	}

	can_recv_sid_cid = lbm_get_current_cid();

	if (timeout > 0.0) {
		lbm_block_ctx_from_extension_timeout(timeout);
	} else {
		lbm_block_ctx_from_extension();
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_can_recv_eid(lbm_value *args, lbm_uint argn) {
	if (argn > 1 || (argn == 1 && !lbm_is_number(args[0]))) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
	}

	float timeout = -1.0;
	if (argn == 1) {
		timeout = lbm_dec_as_float(args[0]);
	}

	can_recv_eid_cid = lbm_get_current_cid();

	if (timeout > 0.0) {
		lbm_block_ctx_from_extension_timeout(timeout);
	} else {
		lbm_block_ctx_from_extension();
	}

	return ENC_SYM_TRUE;
}

// Math

static lbm_value ext_throttle_curve(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(4);
	return lbm_enc_float(utils_throttle_curve(
			lbm_dec_as_float(args[0]),
			lbm_dec_as_float(args[1]),
			lbm_dec_as_float(args[2]),
			lbm_dec_as_i32(args[3])));
}

static lbm_value ext_rand(lbm_value *args, lbm_uint argn) {
	if (argn != 0 && argn != 1) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return ENC_SYM_TERROR;
	}

	unsigned int seed = 0;
	bool seed_set = false;

	if (argn == 1) {
		if (!lbm_is_number(args[0])) {
			lbm_set_error_reason((char*)lbm_error_str_no_number);
			return ENC_SYM_TERROR;
		}

		seed = lbm_dec_as_u32(args[0]);
		seed_set = true;
	}

	if (seed_set) {
		srand(seed);
	}

	return lbm_enc_i32(rand());
}

static lbm_value ext_rand_max(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i32(RAND_MAX);
}

// Bit operations

/*
 * args[0]: Initial value
 * args[1]: Offset in initial value to modify
 * args[2]: Value to modify with
 * args[3]: Size in bits of value to modify with
 */
static lbm_value ext_bits_enc_int(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(4)
	uint32_t initial = lbm_dec_as_u32(args[0]);
	uint32_t offset = lbm_dec_as_u32(args[1]);
	uint32_t number = lbm_dec_as_u32(args[2]);
	uint32_t bits = lbm_dec_as_u32(args[3]);
	initial &= ~((0xFFFFFFFF >> (32 - bits)) << offset);
	initial |= (number << (32 - bits)) >> (32 - bits - offset);

	if (initial > ((1 << 27) - 1)) {
		return lbm_enc_i32(initial);
	} else {
		return lbm_enc_i(initial);
	}
}

/*
 * args[0]: Value
 * args[1]: Offset in initial value to get
 * args[2]: Size in bits of value to get
 */
static lbm_value ext_bits_dec_int(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(3)
	uint32_t val = lbm_dec_as_u32(args[0]);
	uint32_t offset = lbm_dec_as_u32(args[1]);
	uint32_t bits = lbm_dec_as_u32(args[2]);
	val >>= offset;
	val &= 0xFFFFFFFF >> (32 - bits);

	if (val > ((1 << 27) - 1)) {
		return lbm_enc_i32(val);
	} else {
		return lbm_enc_i(val);
	}
}

// Events that will be sent to lisp if a handler is registered

static volatile bool event_can_sid_en = false;
static volatile bool event_can_eid_en = false;
static volatile bool event_data_rx_en = false;
static volatile bool event_shutdown_en = false;
static volatile bool event_icu_width_en = false;
static volatile bool event_icu_period_en = false;
static lbm_uint sym_event_can_sid;
static lbm_uint sym_event_can_eid;
static lbm_uint sym_event_data_rx;
static lbm_uint sym_event_shutdown;
static lbm_uint sym_event_icu_width;
static lbm_uint sym_event_icu_period;

static lbm_value ext_enable_event(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		return ENC_SYM_EERROR;
	}

	if (argn == 2 && !lbm_is_number(args[1])) {
		return ENC_SYM_EERROR;
	}

	bool en = true;
	if (argn == 2 && !lbm_dec_as_i32(args[1])) {
		en = false;
	}

	lbm_uint name = lbm_dec_sym(args[0]);

	if (name == sym_event_can_sid) {
		event_can_sid_en = en;
	} else if (name == sym_event_can_eid) {
		event_can_eid_en = en;
	} else if (name == sym_event_data_rx) {
		event_data_rx_en = en;
	} else if (name == sym_event_shutdown) {
		event_shutdown_en = en;
	} else if (name == sym_event_icu_width) {
		event_icu_width_en = en;
	} else if (name == sym_event_icu_period) {
		event_icu_period_en = en;
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

/*
 * args[0]: Motor, 1 or 2
 * args[1]: Phase, 1, 2 or 3
 * args[2]: Use raw ADC values. Optional argument.
 */
static lbm_value ext_raw_adc_current(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn != 2 && argn != 3) {
		return ENC_SYM_EERROR;
	}

	uint32_t motor = lbm_dec_as_i32(args[0]);
	uint32_t phase = lbm_dec_as_i32(args[1]);

	volatile float ofs1, ofs2, ofs3;
	mcpwm_foc_get_current_offsets(&ofs1, &ofs2, &ofs3, motor == 2);
	float ph1, ph2, ph3;
	mcpwm_foc_get_currents_adc(&ph1, &ph2, &ph3, motor == 2);
	float scale = FAC_CURRENT;

	if (argn == 3 && lbm_dec_as_i32(args[2]) != 0) {
		scale = 1.0;
		ofs1 = 0.0; ofs2 = 0.0; ofs3 = 0.0;
	}

	switch(phase) {
	case 1: return lbm_enc_float((ph1 - ofs1) * scale);
	case 2: return lbm_enc_float((ph2 - ofs2) * scale);
	case 3: return lbm_enc_float((ph3 - ofs3) * scale);
	default: return ENC_SYM_EERROR;
	}
}

/*
 * args[0]: Motor, 1 or 2
 * args[1]: Phase, 1, 2 or 3
 * args[2]: Use raw ADC values. Optional argument.
 */
static lbm_value ext_raw_adc_voltage(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn != 2 && argn != 3) {
		return ENC_SYM_EERROR;
	}

	uint32_t motor = lbm_dec_as_i32(args[0]);
	uint32_t phase = lbm_dec_as_i32(args[1]);

	float ofs1, ofs2, ofs3;
	mcpwm_foc_get_voltage_offsets(&ofs1, &ofs2, &ofs3, motor == 2);
	float scale = ((VIN_R1 + VIN_R2) / VIN_R2) * ADC_VOLTS_PH_FACTOR;

	if (argn == 3 && lbm_dec_as_i32(args[2]) != 0) {
		scale = 4095.0 / V_REG;
		ofs1 = 0.0; ofs2 = 0.0; ofs3 = 0.0;
	}

	float Va = 0.0, Vb = 0.0, Vc = 0.0;
	if (motor == 2) {
#ifdef HW_HAS_DUAL_MOTORS
		Va = (ADC_VOLTS(ADC_IND_SENS4) - ofs1) * scale;
		Vb = (ADC_VOLTS(ADC_IND_SENS5) - ofs2) * scale;
		Vc = (ADC_VOLTS(ADC_IND_SENS6) - ofs3) * scale;
#else
		return ENC_SYM_EERROR;
#endif
	} else if (motor == 1) {
		Va = (ADC_VOLTS(ADC_IND_SENS1) - ofs1) * scale;
		Vb = (ADC_VOLTS(ADC_IND_SENS2) - ofs2) * scale;
		Vc = (ADC_VOLTS(ADC_IND_SENS3) - ofs3) * scale;
	} else {
		return ENC_SYM_EERROR;
	}

	switch(phase) {
	case 1: return lbm_enc_float(Va);
	case 2: return lbm_enc_float(Vb);
	case 3: return lbm_enc_float(Vc);
	default: return ENC_SYM_EERROR;
	}
}

static lbm_value ext_raw_mod_alpha(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_mod_alpha_raw());
}

static lbm_value ext_raw_mod_beta(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_mod_beta_raw());
}

static lbm_value ext_raw_mod_alpha_measured(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_mod_alpha_measured());
}

static lbm_value ext_raw_mod_beta_measured(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mcpwm_foc_get_mod_beta_measured());
}

static lbm_value ext_raw_hall(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_NUMBER_ALL();

	if (argn != 1 && argn != 2) {
		return ENC_SYM_EERROR;
	}

	int motor = lbm_dec_i(args[0]);
	int samples = mc_interface_get_configuration()->m_hall_extra_samples;

	if (argn == 2) {
		samples = lbm_dec_i(args[1]);
	}

	if ((motor != 1 && motor != 2) || samples < 0 || samples > 20) {
		return ENC_SYM_EERROR;
	}

	int hall = utils_read_hall(motor == 2, samples);

	lbm_value hall_list = ENC_SYM_NIL;
	hall_list = lbm_cons(lbm_enc_i((hall >> 2) & 1), hall_list);
	hall_list = lbm_cons(lbm_enc_i((hall >> 1) & 1), hall_list);
	hall_list = lbm_cons(lbm_enc_i((hall >> 0) & 1), hall_list);

	return hall_list;
}

// UART
static SerialConfig uart_cfg = {
		2500000, 0, 0, 0
};

static bool uart_started = false;

static lbm_value ext_uart_start(lbm_value *args, lbm_uint argn) {
	if ((argn != 1 && argn != 2) || !lbm_is_number(args[0])) {
		return ENC_SYM_EERROR;
	}

	int baud = lbm_dec_as_i32(args[0]);

	if (baud < 10 || baud > 10000000) {
		return ENC_SYM_EERROR;
	}

	bool half_duplex = false;
	if (argn == 2) {
		if (lbm_is_symbol(args[1])) {
			if (compare_symbol(lbm_dec_sym(args[1]), &syms_vesc.half_duplex)) {
				half_duplex = true;
			} else {
				return ENC_SYM_EERROR;
			}
		} else {
			return ENC_SYM_TERROR;
		}
	}

	app_configuration *appconf = mempools_alloc_appconf();
	conf_general_read_app_configuration(appconf);
	if (appconf->app_to_use == APP_UART ||
			appconf->app_to_use == APP_PPM_UART ||
			appconf->app_to_use == APP_ADC_UART) {
		appconf->app_to_use = APP_NONE;
		conf_general_store_app_configuration(appconf);
		app_set_configuration(appconf);
	}
	mempools_free_appconf(appconf);

	uart_cfg.speed = baud;
	uart_cfg.cr3 = half_duplex ? USART_CR3_HDSEL : 0;

	sdStop(&HW_UART_DEV);
	sdStart(&HW_UART_DEV, &uart_cfg);

	palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF));
	if (!half_duplex) {
		palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF));
	}

	uart_started = true;

	return ENC_SYM_TRUE;
}

static void wait_uart_tx_task(void *arg) {
	(void)arg;
	while(!chOQIsEmptyI(&HW_UART_DEV.oqueue)){
		chThdSleepMilliseconds(1);
	}
	chThdSleepMilliseconds(1);
	HW_UART_DEV.usart->CR1 |= USART_CR1_RE;
}

static lbm_value ext_uart_write(lbm_value *args, lbm_uint argn) {
	if (argn != 1 || (!lbm_is_cons(args[0]) && !lbm_is_array_r(args[0]))) {
		return ENC_SYM_EERROR;
	}

	if (!uart_started) {
		return ENC_SYM_EERROR;
	}

	const int max_len = 20;
	uint8_t to_send[max_len];
	uint8_t *to_send_ptr = to_send;
	int ind = 0;

	if (lbm_is_array_r(args[0])) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
		to_send_ptr = (uint8_t*)array->data;
		ind = array->size;
	} else {
		lbm_value curr = args[0];
		while (lbm_is_cons(curr)) {
			lbm_value  arg = lbm_car(curr);

			if (lbm_is_number(arg)) {
				to_send[ind++] = lbm_dec_as_u32(arg);
			} else {
				return ENC_SYM_EERROR;
			}

			if (ind == max_len) {
				break;
			}

			curr = lbm_cdr(curr);
		}
	}

	if (uart_cfg.cr3 & USART_CR3_HDSEL) {
		HW_UART_DEV.usart->CR1 &= ~USART_CR1_RE;
		sdWrite(&HW_UART_DEV, to_send_ptr, ind);
		worker_execute(wait_uart_tx_task, 0);
	} else{
		sdWrite(&HW_UART_DEV, to_send_ptr, ind);
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_uart_read(lbm_value *args, lbm_uint argn) {
	if ((argn != 2 && argn != 3 && argn != 4) ||
			!lbm_is_array_r(args[0]) || !lbm_is_number(args[1])) {
		return ENC_SYM_EERROR;
	}

	unsigned int num = lbm_dec_as_u32(args[1]);
	if (num > 512) {
		return ENC_SYM_EERROR;
	}

	if (num == 0 || !uart_started) {
		return lbm_enc_i(0);
	}

	unsigned int offset = 0;
	if (argn >= 3) {
		if (!lbm_is_number(args[2])) {
			return ENC_SYM_EERROR;
		}
		offset = lbm_dec_as_u32(args[2]);
	}

	int stop_at = -1;
	if (argn >= 4) {
		if (!lbm_is_number(args[3])) {
			return ENC_SYM_EERROR;
		}
		stop_at = lbm_dec_as_u32(args[3]);
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	if (array->size < (num + offset)) {
		return ENC_SYM_EERROR;
	}

	unsigned int count = 0;
	msg_t res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
	while (res != MSG_TIMEOUT) {
		((uint8_t*)array->data)[offset + count] = (uint8_t)res;
		count++;
		if (res == stop_at || count >= num) {
			break;
		}
		res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
	}

	return lbm_enc_i(count);
}

static i2c_bb_state i2c_cfg = {
		HW_UART_RX_PORT, HW_UART_RX_PIN,
		HW_UART_TX_PORT, HW_UART_TX_PIN,
		I2C_BB_RATE_400K,
		0,
		0,
		{{NULL, NULL}, NULL, NULL}
};
static bool i2c_started = false;

static lbm_value ext_i2c_start(lbm_value *args, lbm_uint argn) {
	if (argn > 3) {
		return ENC_SYM_EERROR;
	}

	i2c_cfg.rate = I2C_BB_RATE_200K;
	if (argn >= 1) {
		if (!lbm_is_symbol(args[0])) {
			return ENC_SYM_EERROR;
		}

		if (compare_symbol(lbm_dec_sym(args[0]), &syms_vesc.rate_100k)) {
			i2c_cfg.rate = I2C_BB_RATE_100K;
		} else if (compare_symbol(lbm_dec_sym(args[0]), &syms_vesc.rate_200k)) {
			i2c_cfg.rate = I2C_BB_RATE_200K;
		} else if (compare_symbol(lbm_dec_sym(args[0]), &syms_vesc.rate_400k)) {
			i2c_cfg.rate = I2C_BB_RATE_400K;
		} else if (compare_symbol(lbm_dec_sym(args[0]), &syms_vesc.rate_700k)) {
			i2c_cfg.rate = I2C_BB_RATE_700K;
		} else {
			return ENC_SYM_EERROR;
		}
	}

	stm32_gpio_t *sda_gpio = HW_UART_RX_PORT;
	uint32_t sda_pin = HW_UART_RX_PIN;
	if (argn >= 2) {
		if (!lbm_is_symbol(args[1]) ||
				!lispif_symbol_to_io(lbm_dec_sym(args[1]), &sda_gpio, &sda_pin)) {
			return ENC_SYM_EERROR;
		}
	}
	i2c_cfg.sda_gpio = sda_gpio;
	i2c_cfg.sda_pin = sda_pin;

	stm32_gpio_t *scl_gpio = HW_UART_TX_PORT;
	uint32_t scl_pin = HW_UART_TX_PIN;
	if (argn >= 3) {
		if (!lbm_is_symbol(args[2]) ||
				!lispif_symbol_to_io(lbm_dec_sym(args[2]), &scl_gpio, &scl_pin)) {
			return ENC_SYM_EERROR;
		}
	}
	i2c_cfg.scl_gpio = scl_gpio;
	i2c_cfg.scl_pin = scl_pin;

	bool is_using_uart_pins =
			(sda_gpio == HW_UART_TX_PORT && sda_pin == HW_UART_TX_PIN) ||
			(scl_gpio == HW_UART_TX_PORT && scl_pin == HW_UART_TX_PIN) ||
			(sda_gpio == HW_UART_RX_PORT && sda_pin == HW_UART_RX_PIN) ||
			(scl_gpio == HW_UART_RX_PORT && scl_pin == HW_UART_RX_PIN);

	if (is_using_uart_pins) {
		app_configuration *appconf = mempools_alloc_appconf();
		conf_general_read_app_configuration(appconf);

		if (appconf->app_to_use == APP_UART ||
				appconf->app_to_use == APP_PPM_UART ||
				appconf->app_to_use == APP_ADC_UART) {
			appconf->app_to_use = APP_NONE;
			conf_general_store_app_configuration(appconf);
			app_set_configuration(appconf);
		}
		mempools_free_appconf(appconf);
	}

	i2c_bb_init(&i2c_cfg);
	i2c_started = true;

	return ENC_SYM_TRUE;
}

static lbm_value ext_i2c_tx_rx(lbm_value *args, lbm_uint argn) {
	if (argn != 2 && argn != 3) {
		return ENC_SYM_EERROR;
	}

	if (!i2c_started) {
		return lbm_enc_i(0);
	}

	uint16_t addr = 0;
	size_t txlen = 0;
	size_t rxlen = 0;
	uint8_t *txbuf = 0;
	uint8_t *rxbuf = 0;

	const unsigned int max_len = 40;
	uint8_t to_send[max_len];

	if (!lbm_is_number(args[0])) {
		return ENC_SYM_EERROR;
	}
	addr = lbm_dec_as_u32(args[0]);

	if (lbm_is_array_r(args[1])) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
		txbuf = (uint8_t*)array->data;
		txlen = array->size;
	} else {
		lbm_value curr = args[1];
		while (lbm_is_cons(curr)) {
			lbm_value  arg = lbm_car(curr);

			if (lbm_is_number(arg)) {
				to_send[txlen++] = lbm_dec_as_u32(arg);
			} else {
				return ENC_SYM_EERROR;
			}

			if (txlen == max_len) {
				break;
			}

			curr = lbm_cdr(curr);
		}

		if (txlen > 0) {
			txbuf = to_send;
		}
	}

	if (argn >= 3 && lbm_is_array_rw(args[2])) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[2]);
		rxbuf = (uint8_t*)array->data;
		rxlen = array->size;
	}

	i2c_cfg.has_error = false;
	return lbm_enc_i(i2c_bb_tx_rx(&i2c_cfg, addr, txbuf, txlen, rxbuf, rxlen) ? 1 : 0);
}

static lbm_value ext_i2c_restore(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	if (!i2c_started) {
		return lbm_enc_i(0);
	}

	i2c_bb_restore_bus(&i2c_cfg);

	return lbm_enc_i(1);
}

static lbm_value ext_gpio_configure(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN(2);

	if (!lbm_is_symbol(args[0]) || !lbm_is_symbol(args[1])) {
		return ENC_SYM_EERROR;
	}

	lbm_uint name = lbm_dec_sym(args[1]);
	iomode_t mode = PAL_MODE_OUTPUT_PUSHPULL;

	if (compare_symbol(name, &syms_vesc.pin_mode_out)) {
		mode = PAL_MODE_OUTPUT_PUSHPULL;
	} else if (compare_symbol(name, &syms_vesc.pin_mode_od)) {
		mode = PAL_MODE_OUTPUT_OPENDRAIN;
	} else if (compare_symbol(name, &syms_vesc.pin_mode_od_pu)) {
		mode = PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLUP;
	} else if (compare_symbol(name, &syms_vesc.pin_mode_od_pd)) {
		mode = PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLDOWN;
	} else if (compare_symbol(name, &syms_vesc.pin_mode_in)) {
		mode = PAL_MODE_INPUT;
	} else if (compare_symbol(name, &syms_vesc.pin_mode_in_pu)) {
		mode = PAL_MODE_INPUT_PULLUP;
	} else if (compare_symbol(name, &syms_vesc.pin_mode_in_pd)) {
		mode = PAL_MODE_INPUT_PULLDOWN;
	} else if (compare_symbol(name, &syms_vesc.pin_mode_analog)) {
		mode = PAL_STM32_MODE_ANALOG;
	} else {
		return ENC_SYM_EERROR;
	}

	stm32_gpio_t *port; uint32_t pin;
	if (lispif_symbol_to_io(lbm_dec_sym(args[0]), &port, &pin)) {
		palSetPadMode(port, pin, mode);
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_gpio_write(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN(2);

	if (!lbm_is_symbol(args[0]) || !lbm_is_number(args[1])) {
		return ENC_SYM_EERROR;
	}

	stm32_gpio_t *port; uint32_t pin;
	if (lispif_symbol_to_io(lbm_dec_sym(args[0]), &port, &pin)) {
		palWritePad(port, pin, lbm_dec_as_i32(args[1]));
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_gpio_read(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN(1);

	if (!lbm_is_symbol(args[0])) {
		return ENC_SYM_EERROR;
	}

	stm32_gpio_t *port; uint32_t pin;
	if (lispif_symbol_to_io(lbm_dec_sym(args[0]), &port, &pin)) {
		return lbm_enc_i(palReadPad(port, pin));
	} else {
		return ENC_SYM_EERROR;
	}
}

// Configuration

static lbm_value ext_conf_set(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if (argn != 2) {
		return res;
	}

	if (!lbm_is_symbol(args[0])) {
		return res;
	}

	if (!lbm_is_number(args[1])) {
		return res;
	}

	lbm_uint name = lbm_dec_sym(args[0]);

	int changed_mc = 0;
	int changed_app = 0;

	mc_configuration *mcconf = (mc_configuration*)mc_interface_get_configuration();
	app_configuration *appconf = (app_configuration*)app_get_configuration();

	const float speed_fact = ((mcconf->si_motor_poles / 2.0) * 60.0 *
			mcconf->si_gear_ratio) / (mcconf->si_wheel_diameter * M_PI);

	// Safe changes that can be done instantly on the pointer. It is not that good to do
	// it this way, but it is much faster.
	// TODO: Check regularly and make sure that these stay safe.
	if (compare_symbol(name, &syms_vesc.l_current_min)) {
		mcconf->l_current_min = -fabsf(lbm_dec_as_float(args[1]));
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_current_max)) {
		mcconf->l_current_max = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_current_min_scale)) {
		mcconf->l_current_min_scale = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_current_max_scale)) {
		mcconf->l_current_max_scale = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_in_current_min)) {
		mcconf->l_in_current_min = -fabsf(lbm_dec_as_float(args[1]));
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_in_current_max)) {
		mcconf->l_in_current_max = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_abs_current_max)) {
		mcconf->l_abs_current_max = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_min_erpm)) {
		mcconf->l_min_erpm = -fabsf(lbm_dec_as_float(args[1]));
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_max_erpm)) {
		mcconf->l_max_erpm = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_erpm_start)) {
		mcconf->l_erpm_start = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_min_vin)) {
		mcconf->l_min_vin = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_max_vin)) {
		mcconf->l_max_vin = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_min_duty)) {
		mcconf->l_min_duty = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_max_duty)) {
		mcconf->l_max_duty = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.min_speed)) {
		mcconf->l_min_erpm = -fabsf(lbm_dec_as_float(args[1])) * speed_fact;
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.max_speed)) {
		mcconf->l_max_erpm = lbm_dec_as_float(args[1]) * speed_fact;
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_watt_min)) {
		mcconf->l_watt_min = -fabsf(lbm_dec_as_float(args[1]));
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_watt_max)) {
		mcconf->l_watt_max = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_battery_cut_start)) {
		mcconf->l_battery_cut_start = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_battery_cut_end)) {
		mcconf->l_battery_cut_end = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_temp_motor_start)) {
		mcconf->l_temp_motor_start = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_temp_motor_end)) {
		mcconf->l_temp_motor_end = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.l_temp_accel_dec)) {
		mcconf->l_temp_accel_dec = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.bms_limit_mode)) {
		mcconf->bms.limit_mode = lbm_dec_as_i32(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.m_invert_direction)) {
		mcconf->m_invert_direction = lbm_dec_as_i32(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.m_out_aux_mode)) {
		mcconf->m_out_aux_mode = lbm_dec_as_i32(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.m_ntc_motor_beta)) {
		mcconf->m_ntc_motor_beta = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.si_motor_poles)) {
		mcconf->si_motor_poles = lbm_dec_as_i32(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.si_gear_ratio)) {
		mcconf->si_gear_ratio = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.si_wheel_diameter)) {
		mcconf->si_wheel_diameter = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.si_battery_cells)) {
		mcconf->si_battery_cells = lbm_dec_as_i32(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.si_battery_ah)) {
		mcconf->si_battery_ah = lbm_dec_as_float(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.controller_id)) {
		appconf->controller_id = lbm_dec_as_i32(args[1]);
		changed_app = 1;
	}

	// Unsafe changes that require reconfiguration.
	if (changed_mc == 0 && changed_app == 0) {
		mcconf = mempools_alloc_mcconf();
		*mcconf = *mc_interface_get_configuration();

		appconf = mempools_alloc_appconf();
		*appconf = *app_get_configuration();

		if (compare_symbol(name, &syms_vesc.motor_type)) {
			mcconf->motor_type = lbm_dec_as_i32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_sensor_mode)) {
			mcconf->foc_sensor_mode = lbm_dec_as_i32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_current_kp)) {
			mcconf->foc_current_kp = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_current_ki)) {
			mcconf->foc_current_ki = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_f_zv)) {
			mcconf->foc_f_zv = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_motor_l)) {
			mcconf->foc_motor_l = lbm_dec_as_float(args[1]) * 1e-6;
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_motor_ld_lq_diff)) {
			mcconf->foc_motor_ld_lq_diff = lbm_dec_as_float(args[1]) * 1e-6;
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_motor_r)) {
			mcconf->foc_motor_r = lbm_dec_as_float(args[1]) * 1e-3;
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_motor_flux_linkage)) {
			mcconf->foc_motor_flux_linkage = lbm_dec_as_float(args[1]) * 1e-3;
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_observer_gain)) {
			mcconf->foc_observer_gain = lbm_dec_as_float(args[1]) * 1e6;
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hfi_voltage_start)) {
			mcconf->foc_hfi_voltage_start = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hfi_voltage_run)) {
			mcconf->foc_hfi_voltage_run = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hfi_voltage_max)) {
			mcconf->foc_hfi_voltage_max = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm)) {
			mcconf->foc_sl_erpm = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm_start)) {
			mcconf->foc_sl_erpm_start = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t0)) {
			mcconf->foc_hall_table[0] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t1)) {
			mcconf->foc_hall_table[1] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t2)) {
			mcconf->foc_hall_table[2] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t3)) {
			mcconf->foc_hall_table[3] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t4)) {
			mcconf->foc_hall_table[4] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t5)) {
			mcconf->foc_hall_table[5] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t6)) {
			mcconf->foc_hall_table[6] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_hall_t7)) {
			mcconf->foc_hall_table[7] = lbm_dec_as_u32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm_hfi)) {
			mcconf->foc_sl_erpm_hfi = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_openloop_rpm)) {
			mcconf->foc_openloop_rpm = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_openloop_rpm_low)) {
			mcconf->foc_openloop_rpm_low = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_sl_openloop_time_lock)) {
			mcconf->foc_sl_openloop_time_lock = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_sl_openloop_time_ramp)) {
			mcconf->foc_sl_openloop_time_ramp = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_sl_openloop_time)) {
			mcconf->foc_sl_openloop_time = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_temp_comp)) {
			mcconf->foc_temp_comp = lbm_dec_as_i32(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_temp_comp_base_temp)) {
			mcconf->foc_temp_comp_base_temp = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_fw_current_max)) {
			mcconf->foc_fw_current_max = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.foc_fw_duty_start)) {
			mcconf->foc_fw_duty_start = lbm_dec_as_float(args[1]);
			changed_mc = 2;
		} else if (compare_symbol(name, &syms_vesc.can_baud_rate)) {
			appconf->can_baud_rate = lbm_dec_as_i32(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.app_to_use)) {
			appconf->app_to_use = lbm_dec_as_i32(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.ppm_ctrl_type)) {
			appconf->app_ppm_conf.ctrl_type = lbm_dec_as_i32(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.ppm_pulse_start)) {
			appconf->app_ppm_conf.pulse_start = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.ppm_pulse_end)) {
			appconf->app_ppm_conf.pulse_end = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.ppm_pulse_center)) {
			appconf->app_ppm_conf.pulse_center = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.ppm_ramp_time_pos)) {
			appconf->app_ppm_conf.ramp_time_pos = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.ppm_ramp_time_neg)) {
			appconf->app_ppm_conf.ramp_time_neg = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_ctrl_type)) {
			appconf->app_adc_conf.ctrl_type = lbm_dec_as_i32(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_ramp_time_pos)) {
			appconf->app_adc_conf.ramp_time_pos = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_ramp_time_neg)) {
			appconf->app_adc_conf.ramp_time_neg = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_thr_hyst)) {
			appconf->app_adc_conf.hyst = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_v1_start)) {
			appconf->app_adc_conf.voltage_start = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_v1_end)) {
			appconf->app_adc_conf.voltage_end = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_v1_min)) {
			appconf->app_adc_conf.voltage_min = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.adc_v1_max)) {
			appconf->app_adc_conf.voltage_max = lbm_dec_as_float(args[1]);
			changed_app = 2;
		} else if (compare_symbol(name, &syms_vesc.pas_current_scaling)) {
			appconf->app_pas_conf.current_scaling = lbm_dec_as_float(args[1]);
			changed_app = 2;
		}
	}

	if (changed_mc > 0) {
		commands_apply_mcconf_hw_limits(mcconf);
		if (changed_mc == 2) {
			mc_interface_set_configuration(mcconf);
		}
		res = ENC_SYM_TRUE;
	} else if (changed_app > 0) {
		if (changed_app == 2) {
			app_set_configuration(appconf);
		}
		res = ENC_SYM_TRUE;
	} else {
		lbm_set_error_reason("Parameter not recognized");
	}

	if (changed_mc == 2 || changed_app == 2) {
		mempools_free_mcconf(mcconf);
		mempools_free_appconf(appconf);
	}

	return res;
}

static inline float lim_max(float min, float max) { (void)min; return max; }
static inline float lim_min(float min, float max) { (void)max; return min; }

static lbm_value ext_conf_get(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if (argn != 1 && argn != 2) {
		return res;
	}

	if (lbm_type_of(args[0]) != LBM_TYPE_SYMBOL) {
		return res;
	}

	if (argn == 2 && !lbm_is_number(args[1])) {
		return res;
	}

	int defaultcfg = 0;
	if (argn == 2) {
		defaultcfg = lbm_dec_as_i32(args[1]);
	}

	lbm_uint name = lbm_dec_sym(args[0]);

	mc_configuration *mcconf;
	app_configuration *appconf;

	if (defaultcfg) {
		mcconf = mempools_alloc_mcconf();
		appconf = mempools_alloc_appconf();
		confgenerator_set_defaults_mcconf(mcconf);
		confgenerator_set_defaults_appconf(appconf);

		if (defaultcfg == 2) {
#ifdef HW_LIM_CURRENT
			mcconf->l_current_max = lim_max(HW_LIM_CURRENT);
			mcconf->l_current_min = lim_min(HW_LIM_CURRENT);
#else
			mcconf->l_current_max = 500.0;
			mcconf->l_current_min = -500.0;
#endif
#ifdef HW_LIM_CURRENT_IN
			mcconf->l_in_current_max = lim_max(HW_LIM_CURRENT_IN);
			mcconf->l_in_current_min = lim_min(HW_LIM_CURRENT_IN);
#else
			mcconf->l_in_current_max = 500.0;
			mcconf->l_in_current_min = -500.0;
#endif
#ifdef HW_LIM_CURRENT_ABS
			mcconf->l_abs_current_max = lim_max(HW_LIM_CURRENT_ABS);
#else
			mcconf->l_abs_current_max = 500.0;
#endif
#ifdef HW_LIM_VIN
			mcconf->l_max_vin = lim_max(HW_LIM_CURRENT_ABS);
			mcconf->l_min_vin = lim_min(HW_LIM_CURRENT_ABS);
#else
			mcconf->l_max_vin = 100.0;
			mcconf->l_min_vin = 3.0;
#endif
#ifdef HW_LIM_ERPM
			mcconf->l_max_erpm = lim_max(HW_LIM_ERPM);
			mcconf->l_min_erpm = lim_min(HW_LIM_ERPM);
#else
			mcconf->l_max_erpm = 500000.0;
			mcconf->l_min_erpm = -500000.0;
#endif
#ifdef HW_LIM_DUTY_MIN
			mcconf->l_min_duty = lim_max(HW_LIM_DUTY_MIN);
#else
			mcconf->l_min_duty = 0.1;
#endif
#ifdef HW_LIM_DUTY_MAX
			mcconf->l_max_duty = lim_max(HW_LIM_DUTY_MAX);
#else
			mcconf->l_max_duty = 0.98;
#endif
#ifdef HW_LIM_TEMP_FET
			mcconf->l_temp_fet_start = lim_max(HW_LIM_TEMP_FET);
			mcconf->l_temp_fet_end = lim_max(HW_LIM_TEMP_FET);
#else
			mcconf->l_temp_fet_start = 120.0;
			mcconf->l_temp_fet_end = 120.0;
#endif
		}
	} else {
		mcconf = (mc_configuration*)mc_interface_get_configuration();
		appconf = (app_configuration*)app_get_configuration();
	}

	const float speed_fact = ((mcconf->si_motor_poles / 2.0) * 60.0 *
			mcconf->si_gear_ratio) / (mcconf->si_wheel_diameter * M_PI);

	if (compare_symbol(name, &syms_vesc.l_current_min)) {
		res = lbm_enc_float(mcconf->l_current_min);
	} else if (compare_symbol(name, &syms_vesc.l_current_max)) {
		res = lbm_enc_float(mcconf->l_current_max);
	} else if (compare_symbol(name, &syms_vesc.l_current_min_scale)) {
		res = lbm_enc_float(mcconf->l_current_min_scale);
	} else if (compare_symbol(name, &syms_vesc.l_current_max_scale)) {
		res = lbm_enc_float(mcconf->l_current_max_scale);
	} else if (compare_symbol(name, &syms_vesc.l_in_current_min)) {
		res = lbm_enc_float(mcconf->l_in_current_min);
	} else if (compare_symbol(name, &syms_vesc.l_in_current_max)) {
		res = lbm_enc_float(mcconf->l_in_current_max);
	} else if (compare_symbol(name, &syms_vesc.l_abs_current_max)) {
		res = lbm_enc_float(mcconf->l_abs_current_max);
	} else if (compare_symbol(name, &syms_vesc.l_min_erpm)) {
		res = lbm_enc_float(mcconf->l_min_erpm);
	} else if (compare_symbol(name, &syms_vesc.l_max_erpm)) {
		res = lbm_enc_float(mcconf->l_max_erpm);
	} else if (compare_symbol(name, &syms_vesc.l_erpm_start)) {
		res = lbm_enc_float(mcconf->l_erpm_start);
	} else if (compare_symbol(name, &syms_vesc.l_min_vin)) {
		res = lbm_enc_float(mcconf->l_min_vin);
	} else if (compare_symbol(name, &syms_vesc.l_max_vin)) {
		res = lbm_enc_float(mcconf->l_max_vin);
	} else if (compare_symbol(name, &syms_vesc.l_min_duty)) {
		res = lbm_enc_float(mcconf->l_min_duty);
	} else if (compare_symbol(name, &syms_vesc.l_max_duty)) {
		res = lbm_enc_float(mcconf->l_max_duty);
	} else if (compare_symbol(name, &syms_vesc.l_watt_min)) {
		res = lbm_enc_float(mcconf->l_watt_min);
	} else if (compare_symbol(name, &syms_vesc.l_watt_max)) {
		res = lbm_enc_float(mcconf->l_watt_max);
	} else if (compare_symbol(name, &syms_vesc.l_battery_cut_start)) {
		res = lbm_enc_float(mcconf->l_battery_cut_start);
	} else if (compare_symbol(name, &syms_vesc.l_battery_cut_end)) {
		res = lbm_enc_float(mcconf->l_battery_cut_end);
	} else if (compare_symbol(name, &syms_vesc.l_temp_motor_start)) {
		res = lbm_enc_float(mcconf->l_temp_motor_start);
	} else if (compare_symbol(name, &syms_vesc.l_temp_motor_end)) {
		res = lbm_enc_float(mcconf->l_temp_motor_end);
	} else if (compare_symbol(name, &syms_vesc.l_temp_accel_dec)) {
		res = lbm_enc_float(mcconf->l_temp_accel_dec);
	} else if (compare_symbol(name, &syms_vesc.bms_limit_mode)) {
		res = lbm_enc_i(mcconf->bms.limit_mode);
	} else if (compare_symbol(name, &syms_vesc.motor_type)) {
		res = lbm_enc_i(mcconf->motor_type);
	} else if (compare_symbol(name, &syms_vesc.foc_sensor_mode)) {
		res = lbm_enc_i(mcconf->foc_sensor_mode);
	} else if (compare_symbol(name, &syms_vesc.foc_current_kp)) {
		res = lbm_enc_float(mcconf->foc_current_kp);
	} else if (compare_symbol(name, &syms_vesc.foc_current_ki)) {
		res = lbm_enc_float(mcconf->foc_current_ki);
	} else if (compare_symbol(name, &syms_vesc.foc_f_zv)) {
		res = lbm_enc_float(mcconf->foc_f_zv);
	} else if (compare_symbol(name, &syms_vesc.foc_motor_l)) {
		res = lbm_enc_float(mcconf->foc_motor_l * 1e6);
	} else if (compare_symbol(name, &syms_vesc.foc_motor_ld_lq_diff)) {
		res = lbm_enc_float(mcconf->foc_motor_ld_lq_diff * 1e6);
	} else if (compare_symbol(name, &syms_vesc.foc_motor_r)) {
		res = lbm_enc_float(mcconf->foc_motor_r * 1e3);
	} else if (compare_symbol(name, &syms_vesc.foc_motor_flux_linkage)) {
		res = lbm_enc_float(mcconf->foc_motor_flux_linkage * 1e3);
	} else if (compare_symbol(name, &syms_vesc.foc_observer_gain)) {
		res = lbm_enc_float(mcconf->foc_observer_gain * 1e-6);
	} else if (compare_symbol(name, &syms_vesc.foc_hfi_voltage_start)) {
		res = lbm_enc_float(mcconf->foc_hfi_voltage_start);
	} else if (compare_symbol(name, &syms_vesc.foc_hfi_voltage_run)) {
		res = lbm_enc_float(mcconf->foc_hfi_voltage_run);
	} else if (compare_symbol(name, &syms_vesc.foc_hfi_voltage_max)) {
		res = lbm_enc_float(mcconf->foc_hfi_voltage_max);
	} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm)) {
		res = lbm_enc_float(mcconf->foc_sl_erpm);
	} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm_start)) {
		res = lbm_enc_float(mcconf->foc_sl_erpm_start);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t0)) {
		res = lbm_enc_i(mcconf->foc_hall_table[0]);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t1)) {
		res = lbm_enc_i(mcconf->foc_hall_table[1]);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t2)) {
		res = lbm_enc_i(mcconf->foc_hall_table[2]);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t3)) {
		res = lbm_enc_i(mcconf->foc_hall_table[3]);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t4)) {
		res = lbm_enc_i(mcconf->foc_hall_table[4]);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t5)) {
		res = lbm_enc_i(mcconf->foc_hall_table[5]);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t6)) {
		res = lbm_enc_i(mcconf->foc_hall_table[6]);
	} else if (compare_symbol(name, &syms_vesc.foc_hall_t7)) {
		res = lbm_enc_i(mcconf->foc_hall_table[7]);
	} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm_hfi)) {
		res = lbm_enc_float(mcconf->foc_sl_erpm_hfi);
	} else if (compare_symbol(name, &syms_vesc.foc_openloop_rpm)) {
		res = lbm_enc_float(mcconf->foc_openloop_rpm);
	} else if (compare_symbol(name, &syms_vesc.foc_openloop_rpm_low)) {
		res = lbm_enc_float(mcconf->foc_openloop_rpm_low);
	} else if (compare_symbol(name, &syms_vesc.foc_sl_openloop_time_lock)) {
		res = lbm_enc_float(mcconf->foc_sl_openloop_time_lock);
	} else if (compare_symbol(name, &syms_vesc.foc_sl_openloop_time_ramp)) {
		res = lbm_enc_float(mcconf->foc_sl_openloop_time_ramp);
	} else if (compare_symbol(name, &syms_vesc.foc_sl_openloop_time)) {
		res = lbm_enc_float(mcconf->foc_sl_openloop_time);
	} else if (compare_symbol(name, &syms_vesc.foc_temp_comp)) {
		res = lbm_enc_i(mcconf->foc_temp_comp);
	} else if (compare_symbol(name, &syms_vesc.foc_temp_comp_base_temp)) {
		res = lbm_enc_float(mcconf->foc_temp_comp_base_temp);
	} else if (compare_symbol(name, &syms_vesc.foc_fw_current_max)) {
		res = lbm_enc_float(mcconf->foc_fw_current_max);
	} else if (compare_symbol(name, &syms_vesc.foc_fw_duty_start)) {
		res = lbm_enc_float(mcconf->foc_fw_duty_start);
	} else if (compare_symbol(name, &syms_vesc.m_invert_direction)) {
		res = lbm_enc_i(mcconf->m_invert_direction);
	} else if (compare_symbol(name, &syms_vesc.m_out_aux_mode)) {
		res = lbm_enc_i(mcconf->m_out_aux_mode);
	} else if (compare_symbol(name, &syms_vesc.m_ntc_motor_beta)) {
		res = lbm_enc_float(mcconf->m_ntc_motor_beta);
	} else if (compare_symbol(name, &syms_vesc.si_motor_poles)) {
		res = lbm_enc_i(mcconf->si_motor_poles);
	} else if (compare_symbol(name, &syms_vesc.si_gear_ratio)) {
		res = lbm_enc_float(mcconf->si_gear_ratio);
	} else if (compare_symbol(name, &syms_vesc.si_wheel_diameter)) {
		res = lbm_enc_float(mcconf->si_wheel_diameter);
	} else if (compare_symbol(name, &syms_vesc.si_battery_cells)) {
		res = lbm_enc_i(mcconf->si_battery_cells);
	} else if (compare_symbol(name, &syms_vesc.si_battery_ah)) {
		res = lbm_enc_float(mcconf->si_battery_ah);
	} else if (compare_symbol(name, &syms_vesc.min_speed)) {
		res = lbm_enc_float(mcconf->l_min_erpm / speed_fact);
	} else if (compare_symbol(name, &syms_vesc.max_speed)) {
		res = lbm_enc_float(mcconf->l_max_erpm / speed_fact);
	} else if (compare_symbol(name, &syms_vesc.controller_id)) {
		res = lbm_enc_i(appconf->controller_id);
	} else if (compare_symbol(name, &syms_vesc.can_baud_rate)) {
		res = lbm_enc_i(appconf->can_baud_rate);
	} else if (compare_symbol(name, &syms_vesc.app_to_use)) {
		res = lbm_enc_i(appconf->app_to_use);
	} else if (compare_symbol(name, &syms_vesc.ppm_ctrl_type)) {
		res = lbm_enc_i(appconf->app_ppm_conf.ctrl_type);
	} else if (compare_symbol(name, &syms_vesc.ppm_pulse_start)) {
		res = lbm_enc_float(appconf->app_ppm_conf.pulse_start);
	} else if (compare_symbol(name, &syms_vesc.ppm_pulse_end)) {
		res = lbm_enc_float(appconf->app_ppm_conf.pulse_end);
	} else if (compare_symbol(name, &syms_vesc.ppm_pulse_center)) {
		res = lbm_enc_float(appconf->app_ppm_conf.pulse_center);
	} else if (compare_symbol(name, &syms_vesc.ppm_ramp_time_pos)) {
		res = lbm_enc_float(appconf->app_ppm_conf.ramp_time_pos);
	} else if (compare_symbol(name, &syms_vesc.ppm_ramp_time_neg)) {
		res = lbm_enc_float(appconf->app_ppm_conf.ramp_time_neg);
	} else if (compare_symbol(name, &syms_vesc.adc_ctrl_type)) {
		res = lbm_enc_i(appconf->app_adc_conf.ctrl_type);
	} else if (compare_symbol(name, &syms_vesc.adc_ramp_time_pos)) {
		res = lbm_enc_float(appconf->app_adc_conf.ramp_time_pos);
	} else if (compare_symbol(name, &syms_vesc.adc_ramp_time_neg)) {
		res = lbm_enc_float(appconf->app_adc_conf.ramp_time_neg);
	} else if (compare_symbol(name, &syms_vesc.adc_thr_hyst)) {
		res = lbm_enc_float(appconf->app_adc_conf.hyst);
	} else if (compare_symbol(name, &syms_vesc.adc_v1_start)) {
		res = lbm_enc_float(appconf->app_adc_conf.voltage_start);
	} else if (compare_symbol(name, &syms_vesc.adc_v1_end)) {
		res = lbm_enc_float(appconf->app_adc_conf.voltage_end);
	} else if (compare_symbol(name, &syms_vesc.adc_v1_min)) {
		res = lbm_enc_float(appconf->app_adc_conf.voltage_min);
	} else if (compare_symbol(name, &syms_vesc.adc_v1_max)) {
		res = lbm_enc_float(appconf->app_adc_conf.voltage_max);
	} else if (compare_symbol(name, &syms_vesc.pas_current_scaling)) {
		res = lbm_enc_float(appconf->app_pas_conf.current_scaling);
	}

	if (defaultcfg) {
		mempools_free_mcconf(mcconf);
		mempools_free_appconf(appconf);
	}

	if (lbm_type_of(res) == LBM_TYPE_SYMBOL && lbm_dec_sym(res) == SYM_EERROR) {
		lbm_set_error_reason("Parameter not recognized");
	}

	return res;
}

static lbm_value ext_conf_store(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	mc_configuration *mcconf = mempools_alloc_mcconf();
	*mcconf = *mc_interface_get_configuration();
	bool res_mc = conf_general_store_mc_configuration(mcconf, mc_interface_get_motor_thread() == 2);
	mempools_free_mcconf(mcconf);

	app_configuration *appconf = mempools_alloc_appconf();
	*appconf = *app_get_configuration();
	bool res_app = conf_general_store_app_configuration(appconf);
	mempools_free_appconf(appconf);

	return lbm_enc_sym((res_mc && res_app) ? SYM_TRUE : SYM_NIL);
}

typedef struct {
	bool detect_can;
	float max_power_loss;
	float min_current_in;
	float max_current_in;
	float openloop_rpm;
	float sl_erpm;
	lbm_cid id;
	int motor;
} detect_args;

static void detect_task(void *arg) {
	detect_args *a = (detect_args*)arg;
	int restart_cnt = lispif_get_restart_cnt();

	mc_interface_select_motor_thread(a->motor);
	int res = conf_general_detect_apply_all_foc_can(a->detect_can, a->max_power_loss,
			a->min_current_in, a->max_current_in, a->openloop_rpm, a->sl_erpm, NULL);
	mc_interface_select_motor_thread(1);

	if (restart_cnt == lispif_get_restart_cnt()) {
		lbm_unblock_ctx_unboxed(a->id, lbm_enc_i(res));
	}
}

static lbm_value ext_conf_detect_foc(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(6);
	static detect_args a;
	a.detect_can = lbm_dec_as_i32(args[0]);
	a.max_power_loss = lbm_dec_as_float(args[1]);
	a.min_current_in = lbm_dec_as_float(args[2]);
	a.max_current_in = lbm_dec_as_float(args[3]);
	a.openloop_rpm = lbm_dec_as_float(args[4]);
	a.sl_erpm = lbm_dec_as_float(args[5]);
	a.id = lbm_get_current_cid();
	a.motor = mc_interface_get_motor_thread();
	lbm_block_ctx_from_extension();
	worker_execute(detect_task, &a);
	return ENC_SYM_TRUE;
}

static lbm_value ext_conf_set_pid_offset(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return ENC_SYM_TERROR;
	}

	if (!lbm_is_number(args[0]) || (argn == 2 && !is_symbol_true_false(args[1]))) {
		return ENC_SYM_TERROR;
	}

	float angle = lbm_dec_as_float(args[0]);
	if (angle < -360.0 || angle > 360.0) {
		lbm_set_error_reason("Invalid angle. Range should be -360 to 360.");
		return ENC_SYM_TERROR;
	}

	bool store = false;
	if (argn == 2) {
		store = lbm_is_symbol_true(args[1]);
	}

	mc_interface_update_pid_pos_offset(angle, store);

	return ENC_SYM_TRUE;
}

typedef struct {
	float current;
	int samples;
	lbm_cid id;
	int motor;
} measure_res_args;

static void measure_res_task(void *arg) {
	int restart_cnt = lispif_get_restart_cnt();

	measure_res_args *a = (measure_res_args*)arg;
	lbm_flat_value_t v;
	bool ok = false;

	if (lbm_start_flatten(&v, 10)) {
		float res = -1.0;
		mc_interface_select_motor_thread(a->motor);
		mcpwm_foc_measure_resistance(a->current, a->samples, true, &res);
		mc_interface_select_motor_thread(1);

		if (restart_cnt != lispif_get_restart_cnt()) {
			return;
		}

		f_float(&v, res);
		lbm_finish_flatten(&v);
		if (lbm_unblock_ctx(a->id, &v)) {
			ok = true;
		} else {
			lbm_free(v.buf);
		}
	}

	if (!ok) {
		lbm_unblock_ctx_unboxed(a->id, ENC_SYM_NIL);
	}
}

static lbm_value ext_conf_measure_res(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return ENC_SYM_EERROR;
	}

	LBM_CHECK_NUMBER_ALL();

	if (mc_interface_get_configuration()->motor_type != MOTOR_TYPE_FOC) {
		return ENC_SYM_EERROR;
	}

	static measure_res_args a;
	a.current = lbm_dec_as_float(args[0]);
	a.samples = 100;
	if (argn == 2) {
		a.samples = lbm_dec_as_u32(args[1]);
	}
	a.id = lbm_get_current_cid();
	a.motor = mc_interface_get_motor_thread();

	lbm_block_ctx_from_extension();
	worker_execute(measure_res_task, &a);
	return ENC_SYM_TRUE;
}

typedef struct {
	float current;
	int samples;
	lbm_cid id;
	int motor;
} measure_ind_args;

static void measure_inductance_task(void *arg) {
	int restart_cnt = lispif_get_restart_cnt();

	measure_ind_args *a = (measure_ind_args*)arg;
	float ld_lq_avg, ld_lq_diff, real_measurement_current = -1.0;
	int fault;

	lbm_flat_value_t v;
	bool ok = false;
	if (lbm_start_flatten(&v, 25)) {
		mc_interface_select_motor_thread(a->motor);
		fault = mcpwm_foc_measure_inductance_current(a->current, a->samples, &real_measurement_current, &ld_lq_diff, &ld_lq_avg);
		mc_interface_select_motor_thread(1);

		if (restart_cnt != lispif_get_restart_cnt()) {
			return;
		}

		if (fault != 0) {
			f_i(&v, fault);
		} else {
			f_cons(&v);
			f_float(&v, ld_lq_avg);
			f_cons(&v);
			f_float(&v, ld_lq_diff);
			f_cons(&v);
			f_float(&v, real_measurement_current);
			f_sym(&v, SYM_NIL);
		}
		
		lbm_finish_flatten(&v);
		if (lbm_unblock_ctx(a->id, &v)) {
			ok = true;
		} else {
			lbm_free(v.buf);
		}
	}

	if (!ok) {
		lbm_unblock_ctx_unboxed(a->id, ENC_SYM_NIL);
	}
}

static lbm_value ext_conf_measure_ind(lbm_value *args, lbm_uint argn) {
	// measure inductance of motor @ current
	// arg0: measurement current
	// arg1: sample number. optional
	// returns: ({ld_lq_avg} {ld_lq_diff} {actual_measurement_current}) or (fault-code)
	if (argn != 1 && argn != 2) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return ENC_SYM_EERROR;
	}

	LBM_CHECK_NUMBER_ALL();

	if (mc_interface_get_configuration()->motor_type != MOTOR_TYPE_FOC) {
		return ENC_SYM_EERROR;
	}

	static measure_ind_args a;
	a.current = lbm_dec_as_float(args[0]);
	a.samples = 100;
	if (argn == 2) {
		a.samples = lbm_dec_as_u32(args[1]);
	}
	a.id = lbm_get_current_cid();
	a.motor = mc_interface_get_motor_thread();

	if (mc_interface_get_configuration()->l_current_max < a.current) {
		return ENC_SYM_EERROR;
	}

	worker_execute(measure_inductance_task, &a);
	lbm_block_ctx_from_extension();
	return ENC_SYM_TRUE;
}

static lbm_value ext_conf_restore_mc(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	mc_configuration *mcconf = mempools_alloc_mcconf();
	confgenerator_set_defaults_mcconf(mcconf);
	volatile const mc_configuration *mcconf_now = mc_interface_get_configuration();

	// Keep the old offsets
	mcconf->foc_offsets_current[0] = mcconf_now->foc_offsets_current[0];
	mcconf->foc_offsets_current[1] = mcconf_now->foc_offsets_current[1];
	mcconf->foc_offsets_current[2] = mcconf_now->foc_offsets_current[2];
	mcconf->foc_offsets_voltage[0] = mcconf_now->foc_offsets_voltage[0];
	mcconf->foc_offsets_voltage[1] = mcconf_now->foc_offsets_voltage[1];
	mcconf->foc_offsets_voltage[2] = mcconf_now->foc_offsets_voltage[2];
	mcconf->foc_offsets_voltage_undriven[0] = mcconf_now->foc_offsets_voltage_undriven[0];
	mcconf->foc_offsets_voltage_undriven[1] = mcconf_now->foc_offsets_voltage_undriven[1];
	mcconf->foc_offsets_voltage_undriven[2] = mcconf_now->foc_offsets_voltage_undriven[2];

	mc_interface_set_configuration(mcconf);
	mempools_free_mcconf(mcconf);

	return ENC_SYM_TRUE;
}

static lbm_value ext_conf_restore_app(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	app_configuration *appconf = mempools_alloc_appconf();
	confgenerator_set_defaults_appconf(appconf);

	app_set_configuration(appconf);
	timeout_configure(appconf->timeout_msec, appconf->timeout_brake_current, appconf->kill_sw_mode);
	mempools_free_appconf(appconf);

	return ENC_SYM_TRUE;
}

static lbm_value ext_conf_dc_cal(lbm_value *args, lbm_uint argn) {
	if (argn != 1) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return ENC_SYM_TERROR;
	}

	if (!is_symbol_true_false(args[0])) {
		return ENC_SYM_TERROR;
	}

	int cal_res = mcpwm_foc_dc_cal(lbm_is_symbol_true(args[0]));

	if (cal_res < 0) {
		return ENC_SYM_NIL;
	} else {
		volatile const mc_configuration *conf = mc_interface_get_configuration();
		lbm_value res = ENC_SYM_NIL;
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_voltage_undriven[2]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_voltage_undriven[1]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_voltage_undriven[0]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_voltage[2]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_voltage[1]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_voltage[0]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_current[2]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_current[1]), res);
		res = lbm_cons(lbm_enc_float(conf->foc_offsets_current[0]), res);
		return res;
	}
}

static lbm_value ext_conf_get_limits(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	volatile const mc_configuration *conf = mc_interface_get_configuration();
	lbm_value res = ENC_SYM_NIL;
	res = lbm_cons(lbm_enc_float(conf->lo_in_current_max), res);
	res = lbm_cons(lbm_enc_float(conf->lo_in_current_min), res);
	res = lbm_cons(lbm_enc_float(conf->lo_current_max), res);
	res = lbm_cons(lbm_enc_float(conf->lo_current_min), res);
	return res;
}

static lbm_value make_list(int num, ...) {
	va_list arguments;
	va_start (arguments, num);
	lbm_value res = ENC_SYM_NIL;
	for (int i = 0; i < num; i++) {
		res = lbm_cons(va_arg(arguments, lbm_value), res);
	}
	va_end (arguments);
	return lbm_list_destructive_reverse(res);
}

static lbm_uint sym_res;
static lbm_uint sym_loop;
static lbm_uint sym_break;
static lbm_uint sym_brk;
static lbm_uint sym_rst;
static lbm_uint sym_return;

static lbm_value ext_me_defun(lbm_value *argsi, lbm_uint argn) {
	if (argn != 3) {
		return ENC_SYM_TERROR;
	}

	lbm_value name = argsi[0];
	lbm_value args = argsi[1];
	lbm_value body = argsi[2];

	// (define name (lambda args body))

	return make_list(3,
			lbm_enc_sym(SYM_DEFINE),
			name,
			make_list(3,
					lbm_enc_sym(SYM_LAMBDA),
					args,
					body));
}

static lbm_value ext_me_defunret(lbm_value *argsi, lbm_uint argn) {
	if (argn != 3) {
		return ENC_SYM_EERROR;
	}

	lbm_value name = argsi[0];
	lbm_value args = argsi[1];
	lbm_value body = argsi[2];

	// (def name (lambda args (call-cc (lambda (return) body))))

	return make_list(3,
			lbm_enc_sym(SYM_DEFINE),
			name,
			make_list(3,
					lbm_enc_sym(SYM_LAMBDA),
					args,
					make_list(2,
							lbm_enc_sym(SYM_CALLCC),
							make_list(3,
									lbm_enc_sym(SYM_LAMBDA),
									make_list(1, lbm_enc_sym(sym_return)),
									body))));
}

static lbm_value ext_me_loopfor(lbm_value *args, lbm_uint argn) {
	if (argn != 5) {
		return ENC_SYM_EERROR;
	}

	lbm_value it = args[0];
	lbm_value start = args[1];
	lbm_value cond = args[2];
	lbm_value update = args[3];
	lbm_value body = args[4];

	// (let ((loop (lambda (it res break) (if cond (loop update body break) res)))) (call-cc (lambda (brk) (loop start nil brk))))

	return make_list(3,
			lbm_enc_sym(SYM_LET),
			make_list(1,
					make_list(2,
							lbm_enc_sym(sym_loop),
							make_list(3,
									lbm_enc_sym(SYM_LAMBDA),
									make_list(3, it, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
									make_list(4,
											lbm_enc_sym(SYM_IF),
											cond,
											make_list(4, lbm_enc_sym(sym_loop), update, body, lbm_enc_sym(sym_break)),
											lbm_enc_sym(sym_res))))),
											make_list(2,
													lbm_enc_sym(SYM_CALLCC),
													make_list(3,
															lbm_enc_sym(SYM_LAMBDA),
															make_list(1, lbm_enc_sym(sym_brk)),
															make_list(4, lbm_enc_sym(sym_loop), start, ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_loopwhile(lbm_value *args, lbm_uint argn) {
	if (argn != 2) {
		return ENC_SYM_EERROR;
	}

	lbm_value cond = args[0];
	lbm_value body = args[1];

	// (let ((loop (lambda (res break) (if cond (loop body break) res)))) (call-cc (lambda (brk) (loop nil brk))))

	return make_list(3,
			lbm_enc_sym(SYM_LET),
			make_list(1,
					make_list(2,
							lbm_enc_sym(sym_loop),
							make_list(3,
									lbm_enc_sym(SYM_LAMBDA),
									make_list(2, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
									make_list(4,
											lbm_enc_sym(SYM_IF),
											cond,
											make_list(3, lbm_enc_sym(sym_loop), body, lbm_enc_sym(sym_break)),
											lbm_enc_sym(sym_res))))),
											make_list(2,
													lbm_enc_sym(SYM_CALLCC),
													make_list(3,
															lbm_enc_sym(SYM_LAMBDA),
															make_list(1, lbm_enc_sym(sym_brk)),
															make_list(3, lbm_enc_sym(sym_loop), ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_looprange(lbm_value *args, lbm_uint argn) {
	if (argn != 4) {
		return ENC_SYM_EERROR;
	}

	lbm_value it = args[0];
	lbm_value start = args[1];
	lbm_value end = args[2];
	lbm_value body = args[3];

	// (let ((loop (lambda (it res break) (if (< it end) (loop (+ it 1) body break) res)))) (call-cc (lambda (brk) (loop start nil brk))))

	return make_list(3,
			lbm_enc_sym(SYM_LET),
			make_list(1,
					make_list(2,
							lbm_enc_sym(sym_loop),
							make_list(3,
									lbm_enc_sym(SYM_LAMBDA),
									make_list(3, it, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
									make_list(4,
											lbm_enc_sym(SYM_IF),
											make_list(3, lbm_enc_sym(SYM_LT), it, end),
											make_list(4, lbm_enc_sym(sym_loop), make_list(3, lbm_enc_sym(SYM_ADD), it, lbm_enc_i(1)), body, lbm_enc_sym(sym_break)),
											lbm_enc_sym(sym_res))))),
											make_list(2,
													lbm_enc_sym(SYM_CALLCC),
													make_list(3,
															lbm_enc_sym(SYM_LAMBDA),
															make_list(1, lbm_enc_sym(sym_brk)),
															make_list(4, lbm_enc_sym(sym_loop), start, ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_loopforeach(lbm_value *args, lbm_uint argn) {
	if (argn != 3) {
		return ENC_SYM_EERROR;
	}

	lbm_value it = args[0];
	lbm_value lst = args[1];
	lbm_value body = args[2];

	// (let ((loop (lambda (it rst res break) (if (eq it nil) res (loop (car rst) (cdr rst) body break))))) (call-cc (lambda (brk) (loop (car lst) (cdr lst) nil brk))))

	return make_list(3,
			lbm_enc_sym(SYM_LET),
			make_list(1,
					make_list(2,
							lbm_enc_sym(sym_loop),
							make_list(3,
									lbm_enc_sym(SYM_LAMBDA),
									make_list(4, it, lbm_enc_sym(sym_rst), lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
									make_list(4,
											lbm_enc_sym(SYM_IF),
											make_list(3, lbm_enc_sym(SYM_EQ), it, ENC_SYM_NIL),
											lbm_enc_sym(sym_res),
											make_list(5,
													lbm_enc_sym(sym_loop),
													make_list(2, lbm_enc_sym(SYM_CAR), lbm_enc_sym(sym_rst)),
													make_list(2, lbm_enc_sym(SYM_CDR), lbm_enc_sym(sym_rst)),
													body,
													lbm_enc_sym(sym_break))
											)))),
											make_list(2,
													lbm_enc_sym(SYM_CALLCC),
													make_list(3,
															lbm_enc_sym(SYM_LAMBDA),
															make_list(1, lbm_enc_sym(sym_brk)),
															make_list(5,
																	lbm_enc_sym(sym_loop),
																	make_list(2, lbm_enc_sym(SYM_CAR), lst),
																	make_list(2, lbm_enc_sym(SYM_CDR), lst),
																	ENC_SYM_NIL,
																	lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_uavcan_last_rawcmd(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	int can_if = lbm_dec_as_i32(args[0]);
	uavcan_cmd_info info = canard_driver_last_rawcmd(can_if);
	lbm_value out_list = ENC_SYM_NIL;
	out_list = lbm_cons(lbm_enc_float(info.age), out_list);
	out_list = lbm_cons(lbm_enc_float(info.value), out_list);
	return out_list;
}

static lbm_value ext_uavcan_last_rpmcmd(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	int can_if = lbm_dec_as_i32(args[0]);
	uavcan_cmd_info info = canard_driver_last_rpmcmd(can_if);
	lbm_value out_list = ENC_SYM_NIL;
	out_list = lbm_cons(lbm_enc_float(info.age), out_list);
	out_list = lbm_cons(lbm_enc_float(info.value), out_list);
	return out_list;
}

static lbm_value ext_lbm_set_quota(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	uint32_t q = lbm_dec_as_u32(args[0]);

	if (q < 1) {
		return ENC_SYM_EERROR;
	}

	lbm_set_eval_step_quota(q);
	return ENC_SYM_TRUE;
}

lbm_value ext_lbm_set_gc_stack_size(lbm_value *args, lbm_uint argn) {
	if (argn == 1) {
		if (lbm_is_number(args[0])) {
			uint32_t n = lbm_dec_as_u32(args[0]);
			lbm_uint *new_stack = lbm_malloc(n * sizeof(lbm_uint));
			if (new_stack) {
				lbm_free(lbm_heap_state.gc_stack.data);
				lbm_heap_state.gc_stack.data = new_stack;
				lbm_heap_state.gc_stack.size = n;
				lbm_heap_state.gc_stack.max_sp = 0;
				lbm_heap_state.gc_stack.sp = 0;
				return ENC_SYM_TRUE;
			}
			return ENC_SYM_MERROR;
		}
	}
	return ENC_SYM_TERROR;
}

static lbm_value ext_plot_init(lbm_value *args, lbm_uint argn) {
	if (argn != 2) {
		return ENC_SYM_EERROR;
	}

	char *namex = lbm_dec_str(args[0]);
	if (!namex) {
		return ENC_SYM_EERROR;
	}

	char *namey = lbm_dec_str(args[1]);
	if (!namey) {
		return ENC_SYM_EERROR;
	}

	commands_init_plot(namex, namey);

	return ENC_SYM_TRUE;
}

static lbm_value ext_plot_add_graph(lbm_value *args, lbm_uint argn) {
	if (argn != 1) {
		return ENC_SYM_EERROR;
	}

	char *name = lbm_dec_str(args[0]);
	if (!name) {
		return ENC_SYM_EERROR;
	}

	commands_plot_add_graph(name);

	return ENC_SYM_TRUE;
}

static lbm_value ext_plot_set_graph(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	commands_plot_set_graph(lbm_dec_as_i32(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_plot_send_points(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	commands_send_plot_points(
			lbm_dec_as_float(args[0]),
			lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

// IO-boards
static lbm_value ext_ioboard_get_adc(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);

	int id = lbm_dec_as_i32(args[0]);
	int channel = lbm_dec_as_i32(args[1]);

	if (channel < 1 || channel > 8) {
		lbm_set_error_reason("Channel must be 1 - 8");
		return ENC_SYM_EERROR;
	}

	io_board_adc_values *val = 0;
	if (channel >= 5) {
		val = comm_can_get_io_board_adc_5_8_id(id);
		channel -= 4;
	} else {
		val = comm_can_get_io_board_adc_1_4_id(id);
	}

	if (val) {
		return lbm_enc_float(val->adc_voltages[channel - 1]);
	} else {
		return lbm_enc_float(-1.0);
	}
}

static lbm_value ext_ioboard_get_digital(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);

	int id = lbm_dec_as_i32(args[0]);
	int channel = lbm_dec_as_i32(args[1]);

	if (channel < 1 || channel > 64) {
		lbm_set_error_reason("Channel must be 1 - 64");
		return ENC_SYM_EERROR;
	}

	io_board_digial_inputs *val = comm_can_get_io_board_digital_in_id(id);

	if (val) {
		return lbm_enc_i(val->inputs >> (channel - 1));
	} else {
		return lbm_enc_i(-1);
	}
}

static lbm_value ext_ioboard_set_digital(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(3);
	int id = lbm_dec_as_i32(args[0]);
	int channel = lbm_dec_as_i32(args[1]);
	bool on = lbm_dec_as_i32(args[2]);
	comm_can_io_board_set_output_digital(id, channel, on);
	return ENC_SYM_TRUE;
}

static lbm_value ext_ioboard_set_pwm(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(3);
	int id = lbm_dec_as_i32(args[0]);
	int channel = lbm_dec_as_i32(args[1]);
	float duty = lbm_dec_as_float(args[2]);
	comm_can_io_board_set_output_pwm(id, channel, duty);
	return ENC_SYM_TRUE;
}

// Logging
static lbm_value ext_log_start(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN(5);

	if (!lbm_is_number(args[0]) ||
			!lbm_is_number(args[1]) ||
			!lbm_is_number(args[2]) ||
			!is_symbol_true_false(args[3]) ||
			!is_symbol_true_false(args[4])) {
		return ENC_SYM_EERROR;
	}

	log_start(
			lbm_dec_as_i32(args[0]),
			lbm_dec_as_i32(args[1]),
			lbm_dec_as_float(args[2]),
			lbm_is_symbol_true(args[3]),
			lbm_is_symbol_true(args[4]),
			lbm_is_symbol_true(args[4]));

	return ENC_SYM_TRUE;
}

static lbm_value ext_log_stop(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	log_stop(lbm_dec_as_i32(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_log_config_field(lbm_value *args, lbm_uint argn) {
	if (argn != 8) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return ENC_SYM_EERROR;
	}

	int arg_now = 0;

	int can_id = -1;
	if (lbm_is_number(args[arg_now])) {
		can_id = lbm_dec_as_i32(args[arg_now++]);
	} else {
		return ENC_SYM_EERROR;
	}

	int field_ind = -1;
	if (lbm_is_number(args[arg_now])) {
		field_ind = lbm_dec_as_i32(args[arg_now++]);
	} else {
		return ENC_SYM_EERROR;
	}

	char *key = lbm_dec_str(args[arg_now++]);
	if (key == NULL) {
		return ENC_SYM_EERROR;
	}

	char *name = lbm_dec_str(args[arg_now++]);
	if (name == NULL) {
		return ENC_SYM_EERROR;
	}

	char *unit = lbm_dec_str(args[arg_now++]);
	if (unit == NULL) {
		return ENC_SYM_EERROR;
	}

	int precision = -1;
	if (lbm_is_number(args[arg_now])) {
		precision = lbm_dec_as_i32(args[arg_now++]);
	} else {
		return ENC_SYM_EERROR;
	}

	bool is_relative = false;
	if (is_symbol_true_false(args[arg_now])) {
		is_relative = lbm_is_symbol_true(args[arg_now++]);
	} else {
		return ENC_SYM_EERROR;
	}

	bool is_timestamp = false;
	if (is_symbol_true_false(args[arg_now])) {
		is_timestamp = lbm_is_symbol_true(args[arg_now++]);
	} else {
		return ENC_SYM_EERROR;
	}

	log_config_field(can_id, field_ind, key, name, unit, precision, is_relative, is_timestamp);

	return ENC_SYM_TRUE;
}

static lbm_value log_send_fxx(bool is_64, lbm_value *args, lbm_uint argn) {
	unsigned int arg_now = 0;

	int can_id = -1;
	if (lbm_is_number(args[arg_now])) {
		can_id = lbm_dec_as_i32(args[arg_now++]);
	} else {
		return ENC_SYM_EERROR;
	}

	int field_start = -1;
	if (lbm_is_number(args[arg_now])) {
		field_start = lbm_dec_as_i32(args[arg_now++]);
	} else {
		return ENC_SYM_EERROR;
	}

	int32_t ind = 0;
	uint8_t *buffer = mempools_get_packet_buffer();

	buffer[ind++] = is_64 ? COMM_LOG_DATA_F64 : COMM_LOG_DATA_F32;
	buffer_append_int16(buffer, field_start, &ind);

	int append_cnt = 0;
	int append_max = is_64 ? 50 : 100;

	while (arg_now < argn) {
		if (lbm_is_number(args[arg_now])) {
			if (is_64) {
				buffer_append_float64_auto(buffer, lbm_dec_as_double(args[arg_now]), &ind);
			} else {
				buffer_append_float32_auto(buffer, lbm_dec_as_float(args[arg_now]), &ind);
			}
			append_cnt++;
			if (append_cnt >= append_max) {
				mempools_free_packet_buffer(buffer);
				return ENC_SYM_EERROR;
			}
		} else if (lbm_is_cons(args[arg_now])) {
			lbm_value curr = args[arg_now];
			while (lbm_is_cons(curr)) {
				lbm_value  val = lbm_car(curr);
				if (lbm_is_number(val)) {
					if (is_64) {
						buffer_append_float64_auto(buffer, lbm_dec_as_double(val), &ind);
					} else {
						buffer_append_float32_auto(buffer, lbm_dec_as_float(val), &ind);
					}
					append_cnt++;
					if (append_cnt >= append_max) {
						mempools_free_packet_buffer(buffer);
						return ENC_SYM_EERROR;
					}
				} else {
					mempools_free_packet_buffer(buffer);
					return ENC_SYM_EERROR;
				}

				curr = lbm_cdr(curr);
			}
		} else {
			mempools_free_packet_buffer(buffer);
			return ENC_SYM_EERROR;
		}
		arg_now++;
	}

	if (can_id >= 0 && can_id < 255) {
		comm_can_send_buffer(can_id, buffer, ind, 0);
	} else {
		commands_send_packet(buffer, ind);
	}

	mempools_free_packet_buffer(buffer);

	return ENC_SYM_TRUE;
}

static lbm_value ext_log_send_f32(lbm_value *args, lbm_uint argn) {
	return log_send_fxx(false, args, argn);
}

static lbm_value ext_log_send_f64(lbm_value *args, lbm_uint argn) {
	return log_send_fxx(true, args, argn);
}

static lbm_value ext_gnss_lat_lon(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	volatile gnss_data *g = mc_interface_gnss();

	lbm_value lat_lon = ENC_SYM_NIL;
	lat_lon = lbm_cons(lbm_enc_double(g->lon), lat_lon);
	lat_lon = lbm_cons(lbm_enc_double(g->lat), lat_lon);

	return lat_lon;
}

static lbm_value ext_gnss_height(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_gnss()->height);
}

static lbm_value ext_gnss_speed(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_gnss()->speed);
}

static lbm_value ext_gnss_hdop(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_gnss()->hdop);
}

static lbm_value ext_gnss_date_time(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	volatile gnss_data *g = mc_interface_gnss();

	int ms = g->ms_today % 1000;
	int s = (g->ms_today / 1000) % 60;
    int m = ((g->ms_today / 1000) / 60) % 60;
    int h = ((g->ms_today / 1000) / (60 * 60)) % 24;

	lbm_value lat_lon = ENC_SYM_NIL;
	lat_lon = lbm_cons(lbm_enc_i(ms), lat_lon);
	lat_lon = lbm_cons(lbm_enc_i(s), lat_lon);
	lat_lon = lbm_cons(lbm_enc_i(m), lat_lon);
	lat_lon = lbm_cons(lbm_enc_i(h), lat_lon);
	lat_lon = lbm_cons(lbm_enc_i(g->dd), lat_lon);
	lat_lon = lbm_cons(lbm_enc_i(g->mo), lat_lon);
	lat_lon = lbm_cons(lbm_enc_i(g->yy), lat_lon);

	return lat_lon;
}

static lbm_value ext_gnss_age(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(UTILS_AGE_S(mc_interface_gnss()->last_update));
}

static lbm_value ext_empty(lbm_value *args, lbm_uint argn) {
	(void)args;(void)argn;
	return ENC_SYM_TRUE;
}

// Declare native lib extension
lbm_value ext_load_native_lib(lbm_value *args, lbm_uint argn);
lbm_value ext_unload_native_lib(lbm_value *args, lbm_uint argn);

static thread_t *event_tp = NULL;
static THD_WORKING_AREA(event_thread_wa, 256);

// ICU
static volatile uint32_t icu_last_width = 0;
static volatile uint32_t icu_last_period = 0;
static volatile bool icu_width_done = false;
static volatile bool icu_period_done = false;

// Remote Messages
#define RMSG_SLOT_NUM	5

typedef struct {
	lbm_cid cid;
	systime_t start_time;
	float timeout_secs;
} rmsg_state;

static mutex_t rmsg_mutex;
static volatile rmsg_state rmsg_slots[RMSG_SLOT_NUM];

static THD_FUNCTION(event_thread, arg) {
	(void)arg;
	event_tp = chThdGetSelfX();
	chRegSetThreadName("Event Helper");

	for (;;) {
		if (icu_width_done && event_icu_width_en) {
			icu_width_done = false;

			lbm_flat_value_t v;
			if (lbm_start_flatten(&v, 30)) {
				f_cons(&v);
				f_sym(&v, sym_event_icu_width);
				f_cons(&v);
				f_i(&v, icu_last_width);
				f_i(&v, icu_last_period);
				lbm_finish_flatten(&v);
				lbm_event(&v);
			}
		}

		if (icu_period_done && event_icu_period_en) {
			icu_period_done = false;

			lbm_flat_value_t v;
			if (lbm_start_flatten(&v, 30)) {
				f_cons(&v);
				f_sym(&v, sym_event_icu_period);
				f_cons(&v);
				f_i(&v, icu_last_width);
				f_i(&v, icu_last_period);
				lbm_finish_flatten(&v);
				lbm_event(&v);
			}
		}

		chMtxLock(&rmsg_mutex);
		for (int i = 0;i < RMSG_SLOT_NUM;i++) {
			volatile rmsg_state *s = &rmsg_slots[i];
			if (s->cid >= 0 && s->timeout_secs > 0.0 && UTILS_AGE_S(s->start_time) > s->timeout_secs) {
				lbm_unblock_ctx_unboxed(s->cid, ENC_SYM_TIMEOUT);
				s->cid = -1;
			}
		}
		chMtxUnlock(&rmsg_mutex);

		chThdSleepMilliseconds(1);
	}
}

static void icuwidthcb(ICUDriver *icup) {
	icu_last_width = icuGetWidthX(icup);
	icu_last_period = icuGetPeriodX(icup);
	icu_width_done = true;
}

static void icuperiodcb(ICUDriver *icup) {
	icu_last_width = icuGetWidthX(icup);
	icu_last_period = icuGetPeriodX(icup);
	icu_period_done = true;
}

static ICUConfig icucfg = {
		ICU_INPUT_ACTIVE_HIGH,
		1000000,
		icuwidthcb,
		icuperiodcb,
		NULL,
		HW_ICU_CHANNEL,
		0
};

static lbm_value ext_icu_start(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);
	servodec_stop();
	servo_simple_stop();

	if (HW_ICU_DEV.state == ICU_ACTIVE) {
		icuStopCapture(&HW_ICU_DEV);
		icuStop(&HW_ICU_DEV);
	}

	icucfg.frequency = lbm_dec_as_i32(args[0]);
	icucfg.mode = lbm_dec_as_i32(args[1]) ? ICU_INPUT_ACTIVE_HIGH : ICU_INPUT_ACTIVE_LOW;
	icuStart(&HW_ICU_DEV, &icucfg);
	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_ALTERNATE(HW_ICU_GPIO_AF));
	icuStartCapture(&HW_ICU_DEV);
	icuEnableNotifications(&HW_ICU_DEV);
	return ENC_SYM_TRUE;
}

static lbm_value ext_icu_width(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(icu_last_width);
}

static lbm_value ext_icu_period(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(icu_last_period);
}

static lbm_value ext_crc16(lbm_value *args, lbm_uint argn) {
	if ((argn != 1 && argn != 2) || !lbm_is_array_r(args[0])) {
		return ENC_SYM_TERROR;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	unsigned int len = array->size;
	if (argn == 2) {
		if (!lbm_is_number(args[1])) {
			return ENC_SYM_TERROR;
		}

		len = lbm_dec_as_u32(args[1]);
		if (len > array->size) {
			len = array->size;
		}
	}

	return lbm_enc_i(crc16((uint8_t*)array->data, len));
}

static lbm_value ext_crc32(lbm_value *args, lbm_uint argn) {
	if ((argn != 2 && argn != 3) || !lbm_is_array_r(args[0]) || !lbm_is_number(args[1])) {
		return ENC_SYM_TERROR;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	unsigned int len = array->size;
	if (argn == 3) {
		if (!lbm_is_number(args[2])) {
			return ENC_SYM_TERROR;
		}

		len = lbm_dec_as_u32(args[2]);
		if (len > array->size) {
			len = array->size;
		}
	}

	return lbm_enc_u32(crc32_with_init((uint8_t*)array->data, len, lbm_dec_as_u32(args[1])));
}

static lbm_value ext_buf_find(lbm_value *args, lbm_uint argn) {
	if ((argn != 2 && argn != 3) || !lbm_is_array_r(args[0]) || !lbm_is_array_r(args[1])) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
		return ENC_SYM_TERROR;
	}

	lbm_array_header_t *buf = (lbm_array_header_t *)lbm_car(args[0]);
	lbm_array_header_t *seq = (lbm_array_header_t *)lbm_car(args[1]);

	const char* buf_data = (const char*)buf->data;
	const char* seq_data = (const char*)seq->data;

	int res = -1;

	int occurrence = 0;
	if (argn == 3) {
		if (!lbm_is_number(args[2])) {
			lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
			return ENC_SYM_TERROR;
		}

		occurrence = lbm_dec_as_i32(args[2]);
	}

	for (unsigned int i = 0;i < (buf->size - seq->size + 1);i++) {
		bool same = true;

		for (unsigned int j = 0;j < (seq->size - 1);j++) {
			if (buf_data[i + j] != seq_data[j]) {
				same = false;
				break;
			}
		}

		if (same) {
			if (occurrence == 0) {
				res = i;
				break;
			} else {
				occurrence--;
			}
		}
	}

	return lbm_enc_i(res);
}

/**
 * signature: (buf-resize arr:array delta-size:number|nil [new-size:number])
 * -> array
 *
 * If delta-size is passed, this extension calculates the new size by
 * adding the relative size to the current size, otherwise new-size is simply
 * used for the new size.
 *
 * If the new size is smaller than the current size, the array is just shrunk in
 * place without allocating a new buffer. Either delta-size or new-size must not
 * be nil.
 * 
 * Either way, the passed array is always resized mutably, with the returned
 * reference only for convenience.
 */
static lbm_value ext_buf_resize(lbm_value *args, lbm_uint argn) {
	if ((argn != 2 && argn != 3) || !lbm_is_array_rw(args[0])
		|| (!lbm_is_number(args[1]) && !lbm_is_symbol_nil(args[1]))
		|| (argn == 3 && !lbm_is_number(args[2]))) {
		lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
		return ENC_SYM_TERROR;
	}

	bool delta_size_passed = !lbm_is_symbol_nil(args[1]);
	bool new_size_passed   = argn == 3;
	if (!delta_size_passed && !new_size_passed) {
		lbm_set_error_reason(
			"delta-size (arg 2) was nil while new-size wasn't provided (arg 3)"
		);
		return ENC_SYM_EERROR;
	}

	lbm_array_header_t *header = (lbm_array_header_t *)lbm_car(args[0]);
	if (header == NULL) {
		// Should be impossible, unless it contained null pointer to header.
		return ENC_SYM_FATAL_ERROR;
	}
	
	uint32_t new_size;
	{
		int32_t new_size_signed;
		if (delta_size_passed) {
			new_size_signed = header->size + lbm_dec_as_i32(args[1]);
		} else {
			new_size_signed = lbm_dec_as_i32(args[2]);
		}
		
		if (new_size_signed < 0) {
			lbm_set_error_reason("resulting size was negative");
			return ENC_SYM_EERROR;
		}
		new_size = (uint32_t)new_size_signed;
	}
	
	if (new_size == header->size) {
		return args[0];
	} else if (new_size < header->size) {
		uint32_t allocated_size = new_size;
		if (new_size == 0) {
			// arrays of size 0 still need some memory allocated for them.
			allocated_size = 1;
		}
		// We sadly can't trust the return value, as it fails if the allocation
		// was previously a single word long. So we just throw it away.
		lbm_memory_shrink_bytes(header->data, allocated_size);
		
		header->size = new_size;

		return args[0];
	} else {
		void *buffer = lbm_malloc_reserve(new_size);
		if (buffer == NULL) {
			return ENC_SYM_MERROR;
		}

		memcpy(buffer, header->data, header->size);
		memset(buffer + header->size, 0, new_size - header->size);

		lbm_memory_free(header->data);
		header->data = buffer;
		header->size = new_size;

		return args[0];
	}
}

static lbm_value ext_shutdown_hold(lbm_value *args, lbm_uint argn) {
	if (argn != 1) {
		lbm_set_error_reason((char*)lbm_error_str_num_args);
		return ENC_SYM_TERROR;
	}

	if (!is_symbol_true_false(args[0])) {
		return ENC_SYM_TERROR;
	}

	shutdown_hold(lbm_is_symbol_true(args[0]));

	return ENC_SYM_TRUE;
}


// Remote Messages

// (canmsg-recv slot timeout)
static lbm_value ext_canmsg_recv(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(2);

	int slot = lbm_dec_as_i32(args[0]);
	float timeout = lbm_dec_as_float(args[1]);

	if (slot < 0 || slot >= RMSG_SLOT_NUM) {
		return ENC_SYM_TERROR;
	}

	chMtxLock(&rmsg_mutex);
	rmsg_slots[slot].cid = lbm_get_current_cid();
	rmsg_slots[slot].start_time = chVTGetSystemTimeX();
	rmsg_slots[slot].timeout_secs = timeout;
	chMtxUnlock(&rmsg_mutex);

	lbm_block_ctx_from_extension();

	return ENC_SYM_TRUE;
}

// (canmsg-send can-id slot msg)
static lbm_value ext_canmsg_send(lbm_value *args, lbm_uint argn) {
	if (argn != 3 ||
			!lbm_is_number(args[0]) ||
			!lbm_is_number(args[1]) ||
			!lbm_is_array_r(args[2])) {
		lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
		return ENC_SYM_TERROR;
	}

	int can_id = lbm_dec_as_i32(args[0]);
	int slot = lbm_dec_as_i32(args[1]);
	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[2]);

	if (can_id < 0 || can_id > 254) {
		return ENC_SYM_TERROR;
	}

	if (slot < 0 || slot >= RMSG_SLOT_NUM) {
		return ENC_SYM_TERROR;
	}

	if (array == NULL) {
		return ENC_SYM_TERROR;
	}

	if (array->size > 500) {
		return ENC_SYM_TERROR;
	}

	uint8_t *buf = mempools_get_packet_buffer();
	buf[0] = COMM_LISP_RMSG;
	buf[1] = slot;
	memcpy(buf + 2, array->data, array->size);
	comm_can_send_buffer(can_id, buf, array->size + 2, 2);
	mempools_free_packet_buffer(buf);

	return ENC_SYM_TRUE;
}

void lispif_load_vesc_extensions(void) {
	lispif_stop_lib();

	if (event_tp == NULL) {
		chMtxObjectInit(&rmsg_mutex);

		chMtxLock(&rmsg_mutex);
		for (int i = 0;i < RMSG_SLOT_NUM;i++) {
			rmsg_slots[i].cid = -1;
			rmsg_slots[i].timeout_secs = -1.0;
		}
		chMtxUnlock(&rmsg_mutex);

		chThdCreateStatic(event_thread_wa, sizeof(event_thread_wa), NORMALPRIO - 2, event_thread, NULL);
	}

#ifdef HW_ADC_EXT_GPIO
	palSetPadMode(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN, PAL_MODE_INPUT_ANALOG);
#endif
#ifdef HW_ADC_EXT2_GPIO
	palSetPadMode(HW_ADC_EXT2_GPIO, HW_ADC_EXT2_PIN, PAL_MODE_INPUT_ANALOG);
#endif

	lbm_add_symbol_const("hw-esc", &sym_hw_esc);
	lbm_add_symbol_const("event-can-sid", &sym_event_can_sid);
	lbm_add_symbol_const("event-can-eid", &sym_event_can_eid);
	lbm_add_symbol_const("event-data-rx", &sym_event_data_rx);
	lbm_add_symbol_const("event-shutdown", &sym_event_shutdown);
	lbm_add_symbol_const("event-icu-width", &sym_event_icu_width);
	lbm_add_symbol_const("event-icu-period", &sym_event_icu_period);

	lbm_add_symbol_const("a01", &sym_res);
	lbm_add_symbol_const("a02", &sym_loop);
	lbm_add_symbol_const("break", &sym_break);
	lbm_add_symbol_const("a03", &sym_brk);
	lbm_add_symbol_const("a04", &sym_rst);
	lbm_add_symbol_const("return", &sym_return);

	memset(&syms_vesc, 0, sizeof(syms_vesc));

	// Various commands
	lbm_add_extension("print", ext_print);
	lbm_add_extension("puts", ext_puts);
	lbm_add_extension("timeout-reset", ext_reset_timeout);
	lbm_add_extension("get-ppm", ext_get_ppm);
	lbm_add_extension("get-ppm-age", ext_get_ppm_age);
	lbm_add_extension("set-servo", ext_set_servo);
	lbm_add_extension("get-vin", ext_get_vin);
	lbm_add_extension("select-motor", ext_select_motor);
	lbm_add_extension("get-selected-motor", ext_get_selected_motor);
	lbm_add_extension("get-bms-val", ext_get_bms_val);
	lbm_add_extension("set-bms-val", ext_set_bms_val);
	lbm_add_extension("send-bms-can", ext_send_bms_can);
	lbm_add_extension("get-adc", ext_get_adc);
	lbm_add_extension("override-temp-motor", ext_override_temp_motor);
	lbm_add_extension("get-adc-decoded", ext_get_adc_decoded);
	lbm_add_extension("systime", ext_systime);
	lbm_add_extension("secs-since", ext_secs_since);
	lbm_add_extension("set-aux", ext_set_aux);
	lbm_add_extension("event-enable", ext_enable_event);
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
	lbm_add_extension("stats", ext_stats);
	lbm_add_extension("stats-reset", ext_stats_reset);
	lbm_add_extension("import", ext_empty);
	lbm_add_extension("icu-start", ext_icu_start);
	lbm_add_extension("icu-width", ext_icu_width);
	lbm_add_extension("icu-period", ext_icu_period);
	lbm_add_extension("crc16", ext_crc16);
	lbm_add_extension("crc32", ext_crc32);
	lbm_add_extension("buf-find", ext_buf_find);
	lbm_add_extension("buf-resize", ext_buf_resize);
	lbm_add_extension("shutdown-hold", ext_shutdown_hold);

	// APP commands
	lbm_add_extension("app-adc-detach", ext_app_adc_detach);
	lbm_add_extension("app-adc-override", ext_app_adc_override);
	lbm_add_extension("app-adc-range-ok", ext_app_adc_range_ok);
	lbm_add_extension("app-ppm-detach", ext_app_ppm_detach);
	lbm_add_extension("app-ppm-override", ext_app_ppm_override);
	lbm_add_extension("set-remote-state", ext_set_remote_state);
	lbm_add_extension("app-disable-output", ext_app_disable_output);
	lbm_add_extension("app-is-output-disabled", ext_app_is_output_disabled);
	lbm_add_extension("app-pas-get-rpm", ext_app_pas_get_rpm);

	// Motor set commands
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

	lbm_add_extension("foc-beep", ext_foc_beep);
	lbm_add_extension("foc-play-tone", ext_foc_play_tone);
	lbm_add_extension("foc-play-samples", ext_foc_play_samples);
	lbm_add_extension("foc-play-stop", ext_foc_play_stop);

	// Motor get commands
	lbm_add_extension("get-current", ext_get_current);
	lbm_add_extension("get-current-dir", ext_get_current_dir);
	lbm_add_extension("get-current-in", ext_get_current_in);
	lbm_add_extension("get-id", ext_get_id);
	lbm_add_extension("get-iq", ext_get_iq);
	lbm_add_extension("get-id-set", ext_get_id_set);
	lbm_add_extension("get-iq-set", ext_get_iq_set);
	lbm_add_extension("get-vd", ext_get_vd);
	lbm_add_extension("get-vq", ext_get_vq);
	lbm_add_extension("get-est-lambda", ext_foc_est_lambda);
	lbm_add_extension("get-est-res", ext_foc_est_res);
	lbm_add_extension("get-est-ind", ext_foc_est_ind);
	lbm_add_extension("get-duty", ext_get_duty);
	lbm_add_extension("get-rpm", ext_get_rpm);
	lbm_add_extension("get-pos", ext_get_pos);
	lbm_add_extension("get-temp-fet", ext_get_temp_fet);
	lbm_add_extension("get-temp-mot", ext_get_temp_mot);
	lbm_add_extension("get-speed", ext_get_speed);
	lbm_add_extension("get-dist", ext_get_dist);
	lbm_add_extension("get-dist-abs", ext_get_dist_abs);
	lbm_add_extension("get-batt", ext_get_batt);
	lbm_add_extension("get-fault", ext_get_fault);
	lbm_add_extension("get-ah", ext_get_ah);
	lbm_add_extension("get-wh", ext_get_wh);
	lbm_add_extension("get-ah-chg", ext_get_ah_chg);
	lbm_add_extension("get-wh-chg", ext_get_wh_chg);

	// Positions
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

	// Setup values
	lbm_add_extension("setup-ah", ext_setup_ah);
	lbm_add_extension("setup-ah-chg", ext_setup_ah_chg);
	lbm_add_extension("setup-wh", ext_setup_wh);
	lbm_add_extension("setup-wh-chg", ext_setup_wh_chg);
	lbm_add_extension("setup-current", ext_setup_current);
	lbm_add_extension("setup-current-in", ext_setup_current_in);
	lbm_add_extension("setup-num-vescs", ext_setup_num_vescs);

	// CAN-comands
	lbm_add_extension("canset-current", ext_can_current);
	lbm_add_extension("canset-current-rel", ext_can_current_rel);
	lbm_add_extension("canset-duty", ext_can_duty);
	lbm_add_extension("canset-brake", ext_can_brake);
	lbm_add_extension("canset-brake-rel", ext_can_brake_rel);
	lbm_add_extension("canset-rpm", ext_can_rpm);
	lbm_add_extension("canset-pos", ext_can_pos);

	lbm_add_extension("canget-current", ext_can_get_current);
	lbm_add_extension("canget-current-dir", ext_can_get_current_dir);
	lbm_add_extension("canget-current-in", ext_can_get_current_in);
	lbm_add_extension("canget-duty", ext_can_get_duty);
	lbm_add_extension("canget-rpm", ext_can_get_rpm);
	lbm_add_extension("canget-temp-fet", ext_can_get_temp_fet);
	lbm_add_extension("canget-temp-motor", ext_can_get_temp_motor);
	lbm_add_extension("canget-speed", ext_can_get_speed);
	lbm_add_extension("canget-dist", ext_can_get_dist);
	lbm_add_extension("canget-ppm", ext_can_get_ppm);
	lbm_add_extension("canget-adc", ext_can_get_adc);
	lbm_add_extension("canget-vin", ext_can_get_vin);

	lbm_add_extension("can-list-devs", ext_can_list_devs);
	lbm_add_extension("can-scan", ext_can_scan);
	lbm_add_extension("can-send-sid", ext_can_send_sid);
	lbm_add_extension("can-send-eid", ext_can_send_eid);
	lbm_add_extension("can-recv-sid", ext_can_recv_sid);
	lbm_add_extension("can-recv-eid", ext_can_recv_eid);
	lbm_add_extension("can-cmd", ext_can_cmd);
	lbm_add_extension("can-local-id", ext_can_local_id);

	// Math
	lbm_add_extension("throttle-curve", ext_throttle_curve);
	lbm_add_extension("rand", ext_rand);
	lbm_add_extension("rand-max", ext_rand_max);

	// Bit operations
	lbm_add_extension("bits-enc-int", ext_bits_enc_int);
	lbm_add_extension("bits-dec-int", ext_bits_dec_int);

	// Raw readings
	lbm_add_extension("raw-adc-current", ext_raw_adc_current);
	lbm_add_extension("raw-adc-voltage", ext_raw_adc_voltage);
	lbm_add_extension("raw-mod-alpha", ext_raw_mod_alpha);
	lbm_add_extension("raw-mod-beta", ext_raw_mod_beta);
	lbm_add_extension("raw-mod-alpha-measured", ext_raw_mod_alpha_measured);
	lbm_add_extension("raw-mod-beta-measured", ext_raw_mod_beta_measured);
	lbm_add_extension("raw-hall", ext_raw_hall);

	// UART
	uart_started = false;
	lbm_add_extension("uart-start", ext_uart_start);
	lbm_add_extension("uart-write", ext_uart_write);
	lbm_add_extension("uart-read", ext_uart_read);

	// I2C
	i2c_started = false;
	lbm_add_extension("i2c-start", ext_i2c_start);
	lbm_add_extension("i2c-tx-rx", ext_i2c_tx_rx);
	lbm_add_extension("i2c-restore", ext_i2c_restore);

	// GPIO
	lbm_add_extension("gpio-configure", ext_gpio_configure);
	lbm_add_extension("gpio-write", ext_gpio_write);
	lbm_add_extension("gpio-read", ext_gpio_read);

	// Configuration
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
	lbm_add_extension("conf-get-limits", ext_conf_get_limits);

	// Macro expanders
	lbm_add_extension("me-defun", ext_me_defun);
	lbm_add_extension("me-defunret", ext_me_defunret);
	lbm_add_extension("me-loopfor", ext_me_loopfor);
	lbm_add_extension("me-loopwhile", ext_me_loopwhile);
	lbm_add_extension("me-looprange", ext_me_looprange);
	lbm_add_extension("me-loopforeach", ext_me_loopforeach);

	// Native libraries
	lbm_add_extension("load-native-lib", ext_load_native_lib);
	lbm_add_extension("unload-native-lib", ext_unload_native_lib);

	// UAVCAN
	lbm_add_extension("uavcan-last-rawcmd", ext_uavcan_last_rawcmd);
	lbm_add_extension("uavcan-last-rpmcmd", ext_uavcan_last_rpmcmd);

	// Lbm settings
	lbm_add_extension("lbm-set-quota", ext_lbm_set_quota);
	lbm_add_extension("lbm-set-gc-stack-size", ext_lbm_set_gc_stack_size);

	// Plot
	lbm_add_extension("plot-init", ext_plot_init);
	lbm_add_extension("plot-add-graph", ext_plot_add_graph);
	lbm_add_extension("plot-set-graph", ext_plot_set_graph);
	lbm_add_extension("plot-send-points", ext_plot_send_points);

	// IO-boards
	lbm_add_extension("ioboard-get-adc", ext_ioboard_get_adc);
	lbm_add_extension("ioboard-get-digital", ext_ioboard_get_digital);
	lbm_add_extension("ioboard-set-digital", ext_ioboard_set_digital);
	lbm_add_extension("ioboard-set-pwm", ext_ioboard_set_pwm);

	// Logging
	lbm_add_extension("log-start", ext_log_start);
	lbm_add_extension("log-stop", ext_log_stop);
	lbm_add_extension("log-config-field", ext_log_config_field);
	lbm_add_extension("log-send-f32", ext_log_send_f32);
	lbm_add_extension("log-send-f64", ext_log_send_f64);

	// GNSS
	lbm_add_extension("gnss-lat-lon", ext_gnss_lat_lon);
	lbm_add_extension("gnss-height", ext_gnss_height);
	lbm_add_extension("gnss-speed", ext_gnss_speed);
	lbm_add_extension("gnss-hdop", ext_gnss_hdop);
	lbm_add_extension("gnss-date-time", ext_gnss_date_time);
	lbm_add_extension("gnss-age", ext_gnss_age);

	// CAN-Messages
	lbm_add_extension("canmsg-recv", ext_canmsg_recv);
	lbm_add_extension("canmsg-send", ext_canmsg_send);

	// Extra extensions
	lbm_array_extensions_init();
	lbm_math_extensions_init();
	lbm_string_extensions_init();
}

void lispif_process_can(uint32_t can_id, uint8_t *data8, int len, bool is_ext) {
	if (is_ext) {
		if (can_recv_eid_cid < 0 && !event_can_eid_en)  {
			return;
		}
	} else {
		if (can_recv_sid_cid < 0 && !event_can_sid_en)  {
			return;
		}
	}

	lbm_flat_value_t v;
	if (lbm_start_flatten(&v, 50 + len)) {
		f_cons(&v);

		if ((can_recv_sid_cid < 0 && !is_ext) || (can_recv_eid_cid < 0 && is_ext)) {
			f_sym(&v, is_ext ? sym_event_can_eid : sym_event_can_sid);
			f_cons(&v);
			f_i32(&v, can_id);
			f_lbm_array(&v, len, data8);
		} else {
			f_i32(&v, can_id);
			f_cons(&v);
			f_lbm_array(&v, len, data8);
			f_sym(&v, ENC_SYM_NIL);
		}

		lbm_finish_flatten(&v);

		if (can_recv_sid_cid >= 0 && !is_ext) {
			if (!lbm_unblock_ctx(can_recv_sid_cid, &v)) {
				lbm_free(v.buf);
			}
			can_recv_sid_cid = -1;
		} else if (can_recv_eid_cid >= 0 && is_ext) {
			if (!lbm_unblock_ctx(can_recv_eid_cid, &v)) {
				lbm_free(v.buf);
			}
			can_recv_eid_cid = -1;
		} else {
			if (!lbm_event(&v)) {
				lbm_free(v.buf);
			}
		}
	}
}

void lispif_process_custom_app_data(unsigned char *data, unsigned int len) {
	if (!event_data_rx_en && recv_data_cid < 0) {
		return;
	}

	lbm_flat_value_t v;
	if (lbm_start_flatten(&v, 30 + len)) {
		if (recv_data_cid < 0) {
			f_cons(&v);
			f_sym(&v, sym_event_data_rx);
		}
		f_lbm_array(&v, len, data);
		lbm_finish_flatten(&v);

		if (recv_data_cid >= 0) {
			if (!lbm_unblock_ctx(recv_data_cid, &v)) {
				lbm_free(v.buf);
			}
			recv_data_cid = -1;
		} else {
			if (!lbm_event(&v)) {
				lbm_free(v.buf);
			}
		}
	}
}

void lispif_process_shutdown(void) {
	if (!event_shutdown_en) {
		return;
	}

	lbm_flat_value_t v;
	if (lbm_start_flatten(&v, 10)) {
		f_sym(&v, sym_event_shutdown);
		lbm_finish_flatten(&v);
		lbm_event(&v);
	}
}

void lispif_process_rmsg(int slot, unsigned char *data, unsigned int len) {
	if (event_tp == NULL) {
		return;
	}

	chMtxLock(&rmsg_mutex);

	if (slot < 0 || slot >= RMSG_SLOT_NUM || rmsg_slots[slot].cid < 0) {
		chMtxUnlock(&rmsg_mutex);
		return;
	}

	lbm_flat_value_t v;
	if (lbm_start_flatten(&v, 10 + len)) {
		f_lbm_array(&v, len, data);
		lbm_finish_flatten(&v);

		if (!lbm_unblock_ctx(rmsg_slots[slot].cid, &v)) {
			lbm_free(v.buf);
		}

		rmsg_slots[slot].cid = -1;
	}

	chMtxUnlock(&rmsg_mutex);
}

void lispif_disable_all_events(void) {
	if (event_tp != NULL) {
		chMtxLock(&rmsg_mutex);
		for (int i = 0;i < RMSG_SLOT_NUM;i++) {
			rmsg_slots[i].cid = -1;
		}
		chMtxUnlock(&rmsg_mutex);
	}

	lispif_stop_lib();
	event_can_sid_en = false;
	event_can_eid_en = false;
	can_recv_sid_cid = -1;
	can_recv_eid_cid = -1;
	recv_data_cid = -1;
	event_data_rx_en = false;
	event_shutdown_en = false;
	event_icu_width_en = false;
	event_icu_period_en = false;
	// Give thread a chance to stop
	chThdSleepMilliseconds(5);
}

// Note: This function is only safe to use from LBM extensions or while the evaluator is paused
bool lispif_symbol_to_io(lbm_uint sym, stm32_gpio_t **port, uint32_t *pin) {
	if (compare_symbol(sym, &syms_vesc.pin_rx)) {
#ifdef HW_UART_RX_PORT
		*port = HW_UART_RX_PORT; *pin = HW_UART_RX_PIN;
		return true;
#endif
	} else if (compare_symbol(sym, &syms_vesc.pin_tx)) {
#ifdef HW_UART_TX_PORT
		*port = HW_UART_TX_PORT; *pin = HW_UART_TX_PIN;
		return true;
#endif
	} else if (compare_symbol(sym, &syms_vesc.pin_swdio)) {
		*port = GPIOA; *pin = 13;
		return true;
	} else if (compare_symbol(sym, &syms_vesc.pin_swclk)) {
		*port = GPIOA; *pin = 14;
		return true;
	} else if (compare_symbol(sym, &syms_vesc.pin_hall1)) {
		*port = HW_HALL_ENC_GPIO1; *pin = HW_HALL_ENC_PIN1;
		return true;
	} else if (compare_symbol(sym, &syms_vesc.pin_hall2)) {
		*port = HW_HALL_ENC_GPIO2; *pin = HW_HALL_ENC_PIN2;
		return true;
	} else if (compare_symbol(sym, &syms_vesc.pin_hall3)) {
		*port = HW_HALL_ENC_GPIO3; *pin = HW_HALL_ENC_PIN3;
		return true;
	} else if (compare_symbol(sym, &syms_vesc.pin_adc1)) {
#ifdef HW_ADC_EXT_GPIO
		*port = HW_ADC_EXT_GPIO; *pin = HW_ADC_EXT_PIN;
		return true;
#endif
	} else if (compare_symbol(sym, &syms_vesc.pin_adc2)) {
#ifdef HW_ADC_EXT2_GPIO
		*port = HW_ADC_EXT2_GPIO; *pin = HW_ADC_EXT2_PIN;
		return true;
#endif
	} else if (compare_symbol(sym, &syms_vesc.pin_ppm)) {
#ifdef HW_ICU_GPIO
		*port = HW_ICU_GPIO; *pin = HW_ICU_PIN;
		return true;
#endif
	} 
#ifdef PIN_HW_1
	else if (compare_symbol(sym, &syms_vesc.pin_hw_1)) {
		*port = PIN_HW_1_GPIO; *pin = PIN_HW_1;
		return true;
	} 
#endif
#ifdef PIN_HW_2
	else if (compare_symbol(sym, &syms_vesc.pin_hw_2)) {
		*port = PIN_HW_2_GPIO; *pin = PIN_HW_2;
		return true;
	} 
#endif
	return false;
}
