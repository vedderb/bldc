/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se

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

#ifndef DATATYPES_H_
#define DATATYPES_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"

// Data types
typedef enum {
	HW_TYPE_VESC = 0,
	HW_TYPE_VESC_BMS,
	HW_TYPE_CUSTOM_MODULE
} HW_TYPE;

typedef enum {
   MC_STATE_OFF = 0,
   MC_STATE_DETECTING,
   MC_STATE_RUNNING,
   MC_STATE_FULL_BRAKE,
} mc_state;

typedef enum {
	PWM_MODE_NONSYNCHRONOUS_HISW = 0, // This mode is not recommended
	PWM_MODE_SYNCHRONOUS, // The recommended and most tested mode
	PWM_MODE_BIPOLAR // Some glitches occasionally, can kill MOSFETs
} mc_pwm_mode;

typedef enum {
	COMM_MODE_INTEGRATE = 0,
	COMM_MODE_DELAY
} mc_comm_mode;

typedef enum {
	SENSOR_MODE_SENSORLESS = 0,
	SENSOR_MODE_SENSORED,
	SENSOR_MODE_HYBRID
} mc_sensor_mode;

typedef enum {
	FOC_SENSOR_MODE_SENSORLESS = 0,
	FOC_SENSOR_MODE_ENCODER,
	FOC_SENSOR_MODE_HALL,
	FOC_SENSOR_MODE_HFI,
	FOC_SENSOR_MODE_HFI_START,
	FOC_SENSOR_MODE_HFI_V2,
	FOC_SENSOR_MODE_HFI_V3,
	FOC_SENSOR_MODE_HFI_V4,
	FOC_SENSOR_MODE_HFI_V5
} mc_foc_sensor_mode;

typedef enum {
	FOC_CONTROL_SAMPLE_MODE_V0 = 0,
	FOC_CONTROL_SAMPLE_MODE_V0_V7,
	FOC_CONTROL_SAMPLE_MODE_V0_V7_INTERPOL
} mc_foc_control_sample_mode;

typedef enum {
	FOC_CURRENT_SAMPLE_MODE_LONGEST_ZERO = 0,
	FOC_CURRENT_SAMPLE_MODE_ALL_SENSORS,
	FOC_CURRENT_SAMPLE_MODE_HIGH_CURRENT
} mc_foc_current_sample_mode;

// Auxiliary output mode
typedef enum {
	OUT_AUX_MODE_OFF = 0,
	OUT_AUX_MODE_ON_AFTER_2S,
	OUT_AUX_MODE_ON_AFTER_5S,
	OUT_AUX_MODE_ON_AFTER_10S,
	OUT_AUX_MODE_UNUSED,
	OUT_AUX_MODE_ON_WHEN_RUNNING,
	OUT_AUX_MODE_ON_WHEN_NOT_RUNNING,
	OUT_AUX_MODE_MOTOR_50,
	OUT_AUX_MODE_MOSFET_50,
	OUT_AUX_MODE_MOTOR_70,
	OUT_AUX_MODE_MOSFET_70,
	OUT_AUX_MODE_MOTOR_MOSFET_50,
	OUT_AUX_MODE_MOTOR_MOSFET_70,
} out_aux_mode;

// Temperature sensor type
typedef enum {
	TEMP_SENSOR_NTC_10K_25C = 0,
	TEMP_SENSOR_PTC_1K_100C,
	TEMP_SENSOR_KTY83_122,
	TEMP_SENSOR_NTC_100K_25C,
	TEMP_SENSOR_KTY84_130,
	TEMP_SENSOR_NTCX,
	TEMP_SENSOR_PTCX,
	TEMP_SENSOR_PT1000,
	TEMP_SENSOR_DISABLED
} temp_sensor_type;

// General purpose drive output mode
typedef enum {
	GPD_OUTPUT_MODE_NONE = 0,
	GPD_OUTPUT_MODE_MODULATION,
	GPD_OUTPUT_MODE_VOLTAGE,
	GPD_OUTPUT_MODE_CURRENT
} gpd_output_mode;

typedef enum {
	MOTOR_TYPE_BLDC = 0,
	MOTOR_TYPE_DC,
	MOTOR_TYPE_FOC,
	MOTOR_TYPE_GPD
} mc_motor_type;

// FOC current controller decoupling mode.
typedef enum {
	FOC_CC_DECOUPLING_DISABLED = 0,
	FOC_CC_DECOUPLING_CROSS,
	FOC_CC_DECOUPLING_BEMF,
	FOC_CC_DECOUPLING_CROSS_BEMF
} mc_foc_cc_decoupling_mode;

typedef enum {
	FOC_OBSERVER_ORTEGA_ORIGINAL = 0,
	FOC_OBSERVER_MXLEMMING,
	FOC_OBSERVER_ORTEGA_LAMBDA_COMP,
	FOC_OBSERVER_MXLEMMING_LAMBDA_COMP,
} mc_foc_observer_type;

typedef enum {
	FAULT_CODE_NONE = 0,
	FAULT_CODE_OVER_VOLTAGE,
	FAULT_CODE_UNDER_VOLTAGE,
	FAULT_CODE_DRV,
	FAULT_CODE_ABS_OVER_CURRENT,
	FAULT_CODE_OVER_TEMP_FET,
	FAULT_CODE_OVER_TEMP_MOTOR,
	FAULT_CODE_GATE_DRIVER_OVER_VOLTAGE,
	FAULT_CODE_GATE_DRIVER_UNDER_VOLTAGE,
	FAULT_CODE_MCU_UNDER_VOLTAGE,
	FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET,
	FAULT_CODE_ENCODER_SPI,
	FAULT_CODE_ENCODER_SINCOS_BELOW_MIN_AMPLITUDE,
	FAULT_CODE_ENCODER_SINCOS_ABOVE_MAX_AMPLITUDE,
	FAULT_CODE_FLASH_CORRUPTION,
	FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_1,
	FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_2,
	FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_3,
	FAULT_CODE_UNBALANCED_CURRENTS,
	FAULT_CODE_BRK,
	FAULT_CODE_RESOLVER_LOT,
	FAULT_CODE_RESOLVER_DOS,
	FAULT_CODE_RESOLVER_LOS,
	FAULT_CODE_FLASH_CORRUPTION_APP_CFG,
	FAULT_CODE_FLASH_CORRUPTION_MC_CFG,
	FAULT_CODE_ENCODER_NO_MAGNET,
	FAULT_CODE_ENCODER_MAGNET_TOO_STRONG,
	FAULT_CODE_PHASE_FILTER,
	FAULT_CODE_ENCODER_FAULT,
	FAULT_CODE_LV_OUTPUT_FAULT,
	FAULT_CODE_PHASE_OUTPUT_ERROR,
} mc_fault_code;

typedef enum {
	CONTROL_MODE_DUTY = 0,
	CONTROL_MODE_SPEED,
	CONTROL_MODE_CURRENT,
	CONTROL_MODE_CURRENT_BRAKE,
	CONTROL_MODE_POS,
	CONTROL_MODE_HANDBRAKE,
	CONTROL_MODE_OPENLOOP,
	CONTROL_MODE_OPENLOOP_PHASE,
	CONTROL_MODE_OPENLOOP_DUTY,
	CONTROL_MODE_OPENLOOP_DUTY_PHASE,
	CONTROL_MODE_NONE
} mc_control_mode;

typedef enum {
	DISP_POS_MODE_NONE = 0,
	DISP_POS_MODE_INDUCTANCE,
	DISP_POS_MODE_OBSERVER,
	DISP_POS_MODE_ENCODER,
	DISP_POS_MODE_PID_POS,
	DISP_POS_MODE_PID_POS_ERROR,
	DISP_POS_MODE_ENCODER_OBSERVER_ERROR,
	DISP_POS_MODE_HALL_OBSERVER_ERROR
} disp_pos_mode;

typedef enum {
	SENSOR_PORT_MODE_HALL = 0,
	SENSOR_PORT_MODE_ABI,
	SENSOR_PORT_MODE_AS5047_SPI,
	SENSOR_PORT_MODE_AD2S1205,
	SENSOR_PORT_MODE_SINCOS,
	SENSOR_PORT_MODE_TS5700N8501,
	SENSOR_PORT_MODE_TS5700N8501_MULTITURN,
	SENSOR_PORT_MODE_MT6816_SPI_HW,
	SENSOR_PORT_MODE_AS5x47U_SPI,
	SENSOR_PORT_MODE_BISSC,
	SENSOR_PORT_MODE_TLE5012_SSC_SW,
	SENSOR_PORT_MODE_TLE5012_SSC_HW,
	SENSOR_PORT_MODE_CUSTOM_ENCODER,
} sensor_port_mode;

typedef struct {
	float cycle_int_limit;
	float cycle_int_limit_running;
	float cycle_int_limit_max;
	float comm_time_sum;
	float comm_time_sum_min_rpm;
	int32_t comms;
	float time_at_comm;
} mc_rpm_dep_struct;

typedef enum {
	DRV8301_OC_LIMIT = 0,
	DRV8301_OC_LATCH_SHUTDOWN,
	DRV8301_OC_REPORT_ONLY,
	DRV8301_OC_DISABLED
} drv8301_oc_mode;

typedef enum {
	DEBUG_SAMPLING_OFF = 0,
	DEBUG_SAMPLING_NOW,
	DEBUG_SAMPLING_START,
	DEBUG_SAMPLING_TRIGGER_START,
	DEBUG_SAMPLING_TRIGGER_FAULT,
	DEBUG_SAMPLING_TRIGGER_START_NOSEND,
	DEBUG_SAMPLING_TRIGGER_FAULT_NOSEND,
	DEBUG_SAMPLING_SEND_LAST_SAMPLES
} debug_sampling_mode;

typedef enum {
	CAN_BAUD_125K = 0,
	CAN_BAUD_250K,
	CAN_BAUD_500K,
	CAN_BAUD_1M,
	CAN_BAUD_10K,
	CAN_BAUD_20K,
	CAN_BAUD_50K,
	CAN_BAUD_75K,
	CAN_BAUD_100K
} CAN_BAUD;

typedef enum {
	BATTERY_TYPE_LIION_3_0__4_2,
	BATTERY_TYPE_LIIRON_2_6__3_6,
	BATTERY_TYPE_LEAD_ACID
} BATTERY_TYPE;

typedef enum {
	HFI_SAMPLES_8 = 0,
	HFI_SAMPLES_16,
	HFI_SAMPLES_32
} foc_hfi_samples;

typedef enum {
	BMS_TYPE_NONE = 0,
	BMS_TYPE_VESC
} BMS_TYPE;

typedef enum {
	BMS_FWD_CAN_MODE_DISABLED = 0,
	BMS_FWD_CAN_MODE_USB_ONLY,
	BMS_FWD_CAN_MODE_ANY
} BMS_FWD_CAN_MODE;

typedef struct {
	BMS_TYPE type;
	uint8_t limit_mode;
	float t_limit_start;
	float t_limit_end;
	float soc_limit_start;
	float soc_limit_end;
	BMS_FWD_CAN_MODE fwd_can_mode;
} bms_config;

typedef struct {
	float v_tot;
	float v_charge;
	float i_in;
	float i_in_ic;
	float ah_cnt;
	float wh_cnt;
	int cell_num;
	float v_cell[32];
	bool bal_state[32];
	int temp_adc_num;
	float temps_adc[50];
	float temp_ic;
	float temp_hum;
	float pressure;
	float hum;
	float temp_max_cell;
	float soc;
	float soh;
	int can_id;
	float ah_cnt_chg_total;
	float wh_cnt_chg_total;
	float ah_cnt_dis_total;
	float wh_cnt_dis_total;
	systime_t update_time;
} bms_values;

typedef struct {
	int id;
	systime_t rx_time;
	float v_cell_min;
	float v_cell_max;
	float t_cell_max;
	float soc;
	float soh;
	bool is_charging;
	bool is_balancing;
	bool is_charge_allowed;
} bms_soc_soh_temp_stat;

typedef enum {
	PID_RATE_25_HZ = 0,
	PID_RATE_50_HZ,
	PID_RATE_100_HZ,
	PID_RATE_250_HZ,
	PID_RATE_500_HZ,
	PID_RATE_1000_HZ,
	PID_RATE_2500_HZ,
	PID_RATE_5000_HZ,
	PID_RATE_10000_HZ,
} PID_RATE;

typedef enum {
	MTPA_MODE_OFF = 0,
	MTPA_MODE_IQ_TARGET,
	MTPA_MODE_IQ_MEASURED
} MTPA_MODE;

typedef enum {
	FOC_SPEED_SRC_CORRECTED = 0,
	FOC_SPEED_SRC_OBSERVER,
} FOC_SPEED_SRC;

typedef enum {
	S_PID_SPEED_SRC_PLL = 0,
	S_PID_SPEED_SRC_FAST,
	S_PID_SPEED_SRC_FASTER,
} S_PID_SPEED_SRC;

typedef enum {
	SAT_COMP_DISABLED = 0,
	SAT_COMP_FACTOR,
	SAT_COMP_LAMBDA,
	SAT_COMP_LAMBDA_AND_FACTOR
} SAT_COMP_MODE;

typedef struct {
	// Limits
	float l_current_max;
	float l_current_min;
	float l_in_current_max;
	float l_in_current_min;
	float l_in_current_map_start;
	float l_in_current_map_filter;
	float l_abs_current_max;
	float l_min_erpm;
	float l_max_erpm;
	float l_erpm_start;
	float l_max_erpm_fbrake;
	float l_max_erpm_fbrake_cc;
	float l_min_vin;
	float l_max_vin;
	float l_battery_cut_start;
	float l_battery_cut_end;
	float l_battery_regen_cut_start;
	float l_battery_regen_cut_end;
	bool l_slow_abs_current;
	float l_temp_fet_start;
	float l_temp_fet_end;
	float l_temp_motor_start;
	float l_temp_motor_end;
	float l_temp_accel_dec;
	float l_min_duty;
	float l_max_duty;
	float l_watt_max;
	float l_watt_min;
	float l_current_max_scale;
	float l_current_min_scale;
	float l_duty_start;
	// Overridden limits (Computed during runtime)
	float lo_current_max;
	float lo_current_min;
	float lo_in_current_max;
	float lo_in_current_min;

	// BLDC switching and drive
	mc_pwm_mode pwm_mode;
	mc_comm_mode comm_mode;
	mc_motor_type motor_type;
	mc_sensor_mode sensor_mode;

	// Sensorless (bldc)
	float sl_min_erpm;
	float sl_min_erpm_cycle_int_limit;
	float sl_max_fullbreak_current_dir_change;
	float sl_cycle_int_limit;
	float sl_phase_advance_at_br;
	float sl_cycle_int_rpm_br;
	float sl_bemf_coupling_k;
	// Hall sensor
	int8_t hall_table[8];
	float hall_sl_erpm;

	// FOC
	float foc_current_kp;
	float foc_current_ki;
	float foc_f_zv;
	float foc_dt_us;
	float foc_encoder_offset;
	bool foc_encoder_inverted;
	float foc_encoder_ratio;
	float foc_motor_l;
	float foc_motor_ld_lq_diff;
	float foc_motor_r;
	float foc_motor_flux_linkage;
	float foc_observer_gain;
	float foc_observer_gain_slow;
	float foc_observer_offset;
	float foc_pll_kp;
	float foc_pll_ki;
	float foc_duty_dowmramp_kp;
	float foc_duty_dowmramp_ki;
	float foc_start_curr_dec;
	float foc_start_curr_dec_rpm;
	float foc_openloop_rpm;
	float foc_openloop_rpm_low;
	float foc_d_gain_scale_start;
	float foc_d_gain_scale_max_mod;
	float foc_sl_openloop_hyst;
	float foc_sl_openloop_time;
	float foc_sl_openloop_time_lock;
	float foc_sl_openloop_time_ramp;
	float foc_sl_openloop_boost_q;
	float foc_sl_openloop_max_q;
	mc_foc_sensor_mode foc_sensor_mode;
	uint8_t foc_hall_table[8];
	float foc_hall_interp_erpm;
	float foc_sl_erpm_start;
	float foc_sl_erpm;
	mc_foc_control_sample_mode foc_control_sample_mode;
	mc_foc_current_sample_mode foc_current_sample_mode;
	SAT_COMP_MODE foc_sat_comp_mode;
	float foc_sat_comp;
	bool foc_temp_comp;
	float foc_temp_comp_base_temp;
	float foc_current_filter_const;
	mc_foc_cc_decoupling_mode foc_cc_decoupling;
	mc_foc_observer_type foc_observer_type;
	float foc_hfi_voltage_start;
	float foc_hfi_voltage_run;
	float foc_hfi_voltage_max;
	float foc_hfi_gain;
	float foc_hfi_max_err;
	float foc_hfi_hyst;
	float foc_sl_erpm_hfi;
	uint16_t foc_hfi_start_samples;
	float foc_hfi_obs_ovr_sec;
	foc_hfi_samples foc_hfi_samples;
	bool foc_offsets_cal_on_boot;
	float foc_offsets_current[3];
	float foc_offsets_voltage[3];
	float foc_offsets_voltage_undriven[3];
	bool foc_phase_filter_enable;
	bool foc_phase_filter_disable_fault;
	float foc_phase_filter_max_erpm;
	MTPA_MODE foc_mtpa_mode;
	// Field Weakening
	float foc_fw_current_max;
	float foc_fw_duty_start;
	float foc_fw_ramp_time;
	float foc_fw_q_current_factor;
	FOC_SPEED_SRC foc_speed_soure;

	// GPDrive
	int gpd_buffer_notify_left;
	int gpd_buffer_interpol;
	float gpd_current_filter_const;
	float gpd_current_kp;
	float gpd_current_ki;

	PID_RATE sp_pid_loop_rate;

	// Speed PID
	float s_pid_kp;
	float s_pid_ki;
	float s_pid_kd;
	float s_pid_kd_filter;
	float s_pid_min_erpm;
	bool s_pid_allow_braking;
	float s_pid_ramp_erpms_s;
	S_PID_SPEED_SRC s_pid_speed_source;

	// Pos PID
	float p_pid_kp;
	float p_pid_ki;
	float p_pid_kd;
	float p_pid_kd_proc;
	float p_pid_kd_filter;
	float p_pid_ang_div;
	float p_pid_gain_dec_angle;
	float p_pid_offset;

	// Current controller
	float cc_startup_boost_duty;
	float cc_min_current;
	float cc_gain;
	float cc_ramp_step_max;

	// Misc
	int32_t m_fault_stop_time_ms;
	float m_duty_ramp_step;
	float m_current_backoff_gain;
	uint32_t m_encoder_counts;
	float m_encoder_sin_offset;
	float m_encoder_sin_amp;
	float m_encoder_cos_offset;
	float m_encoder_cos_amp;
	float m_encoder_sincos_filter_constant;
	float m_encoder_sincos_phase_correction;
	sensor_port_mode m_sensor_port_mode;
	bool m_invert_direction;
	drv8301_oc_mode m_drv8301_oc_mode;
	int m_drv8301_oc_adj;
	float m_bldc_f_sw_min;
	float m_bldc_f_sw_max;
	float m_dc_f_sw;
	float m_ntc_motor_beta;
	out_aux_mode m_out_aux_mode;
	temp_sensor_type m_motor_temp_sens_type;
	float m_ptc_motor_coeff;
	int m_hall_extra_samples;
	int m_batt_filter_const;
	float m_ntcx_ptcx_temp_base;
	float m_ntcx_ptcx_res;
	// Setup info
	uint8_t si_motor_poles;
	float si_gear_ratio;
	float si_wheel_diameter;
	BATTERY_TYPE si_battery_type;
	int si_battery_cells;
	float si_battery_ah;
	float si_motor_nl_current;

	// BMS Configuration
	bms_config bms;

	// Protect from flash corruption.
	uint16_t crc;
} mc_configuration;

// Applications to use
typedef enum {
	APP_NONE = 0,
	APP_PPM,
	APP_ADC,
	APP_UART,
	APP_PPM_UART,
	APP_ADC_UART,
	APP_NUNCHUK,
	APP_NRF,
	APP_CUSTOM,
	APP_PAS,
	APP_ADC_PAS
} app_use;

// Throttle curve mode
typedef enum {
	THR_EXP_EXPO = 0,
	THR_EXP_NATURAL,
	THR_EXP_POLY
} thr_exp_mode;

typedef enum {
	SAFE_START_DISABLED = 0,
	SAFE_START_REGULAR,
	SAFE_START_NO_FAULT
} SAFE_START_MODE;

// PPM control types
typedef enum {
	PPM_CTRL_TYPE_NONE = 0,
	PPM_CTRL_TYPE_CURRENT,
	PPM_CTRL_TYPE_CURRENT_NOREV,
	PPM_CTRL_TYPE_CURRENT_NOREV_BRAKE,
	PPM_CTRL_TYPE_DUTY,
	PPM_CTRL_TYPE_DUTY_NOREV,
	PPM_CTRL_TYPE_PID,
	PPM_CTRL_TYPE_PID_NOREV,
	PPM_CTRL_TYPE_CURRENT_BRAKE_REV_HYST,
	PPM_CTRL_TYPE_CURRENT_SMART_REV,
	PPM_CTRL_TYPE_PID_POSITION_180,
	PPM_CTRL_TYPE_PID_POSITION_360,
} ppm_control_type;

typedef struct {
	ppm_control_type ctrl_type;
	float pid_max_erpm;
	float hyst;
	float pulse_start;
	float pulse_end;
	float pulse_center;
	bool median_filter;
	SAFE_START_MODE safe_start;
	float throttle_exp;
	float throttle_exp_brake;
	thr_exp_mode throttle_exp_mode;
	float ramp_time_pos;
	float ramp_time_neg;
	bool multi_esc;
	bool tc;
	float tc_max_diff;
	float max_erpm_for_dir;
	float smart_rev_max_duty;
	float smart_rev_ramp_time;
} ppm_config;

// ADC control types
typedef enum {
	ADC_CTRL_TYPE_NONE = 0,
	ADC_CTRL_TYPE_CURRENT,
	ADC_CTRL_TYPE_CURRENT_REV_CENTER,
	ADC_CTRL_TYPE_CURRENT_REV_BUTTON,
	ADC_CTRL_TYPE_CURRENT_REV_BUTTON_BRAKE_ADC,
	ADC_CTRL_TYPE_CURRENT_REV_BUTTON_BRAKE_CENTER,
	ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_CENTER,
	ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_BUTTON,
	ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_ADC,
	ADC_CTRL_TYPE_DUTY,
	ADC_CTRL_TYPE_DUTY_REV_CENTER,
	ADC_CTRL_TYPE_DUTY_REV_BUTTON,
	ADC_CTRL_TYPE_PID,
	ADC_CTRL_TYPE_PID_REV_CENTER,
	ADC_CTRL_TYPE_PID_REV_BUTTON
} adc_control_type;

// PAS control types
typedef enum {
	PAS_CTRL_TYPE_NONE = 0,
	PAS_CTRL_TYPE_CADENCE,
	PAS_CTRL_TYPE_TORQUE,
	PAS_CTRL_TYPE_TORQUE_WITH_CADENCE_TIMEOUT
} pas_control_type;

// PAS sensor types
typedef enum {
	PAS_SENSOR_TYPE_QUADRATURE = 0
} pas_sensor_type;

typedef struct {
	adc_control_type ctrl_type;
	float hyst;
	float voltage_start;
	float voltage_end;
	float voltage_min;
	float voltage_max;
	float voltage_center;
	float voltage2_start;
	float voltage2_end;
	bool use_filter;
	SAFE_START_MODE safe_start;
	uint8_t buttons;
	bool voltage_inverted;
	bool voltage2_inverted;
	float throttle_exp;
	float throttle_exp_brake;
	thr_exp_mode throttle_exp_mode;
	float ramp_time_pos;
	float ramp_time_neg;
	bool multi_esc;
	bool tc;
	float tc_max_diff;
	uint32_t update_rate_hz;
} adc_config;

// Nunchuk control types
typedef enum {
	CHUK_CTRL_TYPE_NONE = 0,
	CHUK_CTRL_TYPE_CURRENT,
	CHUK_CTRL_TYPE_CURRENT_NOREV,
	CHUK_CTRL_TYPE_CURRENT_BIDIRECTIONAL
} chuk_control_type;

typedef struct {
	chuk_control_type ctrl_type;
	float hyst;
	float ramp_time_pos;
	float ramp_time_neg;
	float stick_erpm_per_s_in_cc;
	float throttle_exp;
	float throttle_exp_brake;
	thr_exp_mode throttle_exp_mode;
	bool multi_esc;
	bool tc;
	float tc_max_diff;
	bool use_smart_rev;
	float smart_rev_max_duty;
	float smart_rev_ramp_time;
} chuk_config;

typedef struct {
	pas_control_type ctrl_type;
	pas_sensor_type sensor_type;
	float current_scaling;
	float pedal_rpm_start;
	float pedal_rpm_end;
	bool invert_pedal_direction;
	uint8_t magnets;
	bool use_filter;
	float ramp_time_pos;
	float ramp_time_neg;
	uint32_t update_rate_hz;
} pas_config;

// NRF Datatypes
typedef enum {
	NRF_SPEED_250K = 0,
	NRF_SPEED_1M,
	NRF_SPEED_2M
} NRF_SPEED;

typedef enum {
	NRF_POWER_M18DBM = 0,
	NRF_POWER_M12DBM,
	NRF_POWER_M6DBM,
	NRF_POWER_0DBM,
  NRF_POWER_OFF
} NRF_POWER;

typedef enum {
	NRF_AW_3 = 0,
	NRF_AW_4,
	NRF_AW_5
} NRF_AW;

typedef enum {
	NRF_CRC_DISABLED = 0,
	NRF_CRC_1B,
	NRF_CRC_2B
} NRF_CRC;

typedef enum {
	NRF_RETR_DELAY_250US = 0,
	NRF_RETR_DELAY_500US,
	NRF_RETR_DELAY_750US,
	NRF_RETR_DELAY_1000US,
	NRF_RETR_DELAY_1250US,
	NRF_RETR_DELAY_1500US,
	NRF_RETR_DELAY_1750US,
	NRF_RETR_DELAY_2000US,
	NRF_RETR_DELAY_2250US,
	NRF_RETR_DELAY_2500US,
	NRF_RETR_DELAY_2750US,
	NRF_RETR_DELAY_3000US,
	NRF_RETR_DELAY_3250US,
	NRF_RETR_DELAY_3500US,
	NRF_RETR_DELAY_3750US,
	NRF_RETR_DELAY_4000US
} NRF_RETR_DELAY;

typedef struct {
	NRF_SPEED speed;
	NRF_POWER power;
	NRF_CRC crc_type;
	NRF_RETR_DELAY retry_delay;
	unsigned char retries;
	unsigned char channel;
	unsigned char address[3];
	bool send_crc_ack;
} nrf_config;

typedef enum {
	SHUTDOWN_MODE_ALWAYS_OFF = 0,
	SHUTDOWN_MODE_ALWAYS_ON,
	SHUTDOWN_MODE_TOGGLE_BUTTON_ONLY,
	SHUTDOWN_MODE_OFF_AFTER_10S,
	SHUTDOWN_MODE_OFF_AFTER_1M,
	SHUTDOWN_MODE_OFF_AFTER_5M,
	SHUTDOWN_MODE_OFF_AFTER_10M,
	SHUTDOWN_MODE_OFF_AFTER_30M,
	SHUTDOWN_MODE_OFF_AFTER_1H,
	SHUTDOWN_MODE_OFF_AFTER_5H,
} SHUTDOWN_MODE;

typedef enum {
	IMU_TYPE_OFF = 0,
	IMU_TYPE_INTERNAL,
	IMU_TYPE_EXTERNAL_MPU9X50,
	IMU_TYPE_EXTERNAL_ICM20948,
	IMU_TYPE_EXTERNAL_BMI160,
	IMU_TYPE_EXTERNAL_LSM6DS3
} IMU_TYPE;

typedef enum {
	AHRS_MODE_MADGWICK = 0,
	AHRS_MODE_MAHONY,
	AHRS_MODE_MADGWICK_FUSION
} AHRS_MODE;

typedef enum {
	IMU_FILTER_LOW = 0,
	IMU_FILTER_MEDIUM,
	IMU_FILTER_HIGH
} IMU_FILTER;

typedef struct {
	IMU_TYPE type;
	AHRS_MODE mode;
	IMU_FILTER filter;
	float accel_lowpass_filter_x;
	float accel_lowpass_filter_y;
	float accel_lowpass_filter_z;
	float gyro_lowpass_filter;
	int sample_rate_hz;
	bool use_magnetometer;
	float accel_confidence_decay;
	float mahony_kp;
	float mahony_ki;
	float madgwick_beta;
	float rot_roll;
	float rot_pitch;
	float rot_yaw;
	float accel_offsets[3];
	float gyro_offsets[3];
} imu_config;

typedef enum {
	CAN_MODE_VESC = 0,
	CAN_MODE_UAVCAN,
	CAN_MODE_COMM_BRIDGE,
	CAN_MODE_UNUSED,
} CAN_MODE;

typedef enum {
	UAVCAN_RAW_MODE_CURRENT = 0,
	UAVCAN_RAW_MODE_CURRENT_NO_REV_BRAKE,
	UAVCAN_RAW_MODE_DUTY,
	UAVCAN_RAW_MODE_RPM
} UAVCAN_RAW_MODE;

typedef enum {
	UAVCAN_STATUS_CURRENT_MODE_MOTOR = 0,
	UAVCAN_STATUS_CURRENT_MODE_INPUT
} UAVCAN_STATUS_CURRENT_MODE;

typedef enum {
	KILL_SW_MODE_DISABLED = 0,
	KILL_SW_MODE_PPM_LOW,
	KILL_SW_MODE_PPM_HIGH,
	KILL_SW_MODE_ADC2_LOW,
	KILL_SW_MODE_ADC2_HIGH
} KILL_SW_MODE;

typedef struct {
	// Settings
	uint8_t controller_id;
	uint32_t timeout_msec;
	float timeout_brake_current;
	uint32_t can_status_rate_1;
	uint8_t can_status_msgs_r1;
	uint32_t can_status_rate_2;
	uint8_t can_status_msgs_r2;
	CAN_BAUD can_baud_rate;
	bool pairing_done;
	bool permanent_uart_enabled;
	SHUTDOWN_MODE shutdown_mode;
	bool servo_out_enable;
	KILL_SW_MODE kill_sw_mode;

	// CAN modes
	CAN_MODE can_mode;
	uint8_t uavcan_esc_index;
	UAVCAN_RAW_MODE uavcan_raw_mode;
	float uavcan_raw_rpm_max;
	UAVCAN_STATUS_CURRENT_MODE uavcan_status_current_mode;

	// Application to use
	app_use app_to_use;

	// PPM application settings
	ppm_config app_ppm_conf;

	// ADC application settings
	adc_config app_adc_conf;

	// UART application settings
	uint32_t app_uart_baudrate;

	// Nunchuk application settings
	chuk_config app_chuk_conf;

	// NRF application settings
	nrf_config app_nrf_conf;

	// Pedal Assist application settings
	pas_config app_pas_conf;

	// IMU Settings
	imu_config imu_conf;

	// Protect from flash corruption
	uint16_t crc;
} app_configuration;

// Communication commands
typedef enum {
	COMM_FW_VERSION							= 0,
	COMM_JUMP_TO_BOOTLOADER					= 1,
	COMM_ERASE_NEW_APP						= 2,
	COMM_WRITE_NEW_APP_DATA					= 3,
	COMM_GET_VALUES							= 4,
	COMM_SET_DUTY							= 5,
	COMM_SET_CURRENT						= 6,
	COMM_SET_CURRENT_BRAKE					= 7,
	COMM_SET_RPM							= 8,
	COMM_SET_POS							= 9,
	COMM_SET_HANDBRAKE						= 10,
	COMM_SET_DETECT							= 11,
	COMM_SET_SERVO_POS						= 12,
	COMM_SET_MCCONF							= 13,
	COMM_GET_MCCONF							= 14,
	COMM_GET_MCCONF_DEFAULT					= 15,
	COMM_SET_APPCONF						= 16,
	COMM_GET_APPCONF						= 17,
	COMM_GET_APPCONF_DEFAULT				= 18,
	COMM_SAMPLE_PRINT						= 19,
	COMM_TERMINAL_CMD						= 20,
	COMM_PRINT								= 21,
	COMM_ROTOR_POSITION						= 22,
	COMM_EXPERIMENT_SAMPLE					= 23,
	COMM_DETECT_MOTOR_PARAM					= 24,
	COMM_DETECT_MOTOR_R_L					= 25,
	COMM_DETECT_MOTOR_FLUX_LINKAGE			= 26,
	COMM_DETECT_ENCODER						= 27,
	COMM_DETECT_HALL_FOC					= 28,
	COMM_REBOOT								= 29,
	COMM_ALIVE								= 30,
	COMM_GET_DECODED_PPM					= 31,
	COMM_GET_DECODED_ADC					= 32,
	COMM_GET_DECODED_CHUK					= 33,
	COMM_FORWARD_CAN						= 34,
	COMM_SET_CHUCK_DATA						= 35,
	COMM_CUSTOM_APP_DATA					= 36,
	COMM_NRF_START_PAIRING					= 37,
	COMM_GPD_SET_FSW						= 38,
	COMM_GPD_BUFFER_NOTIFY					= 39,
	COMM_GPD_BUFFER_SIZE_LEFT				= 40,
	COMM_GPD_FILL_BUFFER					= 41,
	COMM_GPD_OUTPUT_SAMPLE					= 42,
	COMM_GPD_SET_MODE						= 43,
	COMM_GPD_FILL_BUFFER_INT8				= 44,
	COMM_GPD_FILL_BUFFER_INT16				= 45,
	COMM_GPD_SET_BUFFER_INT_SCALE			= 46,
	COMM_GET_VALUES_SETUP					= 47,
	COMM_SET_MCCONF_TEMP					= 48,
	COMM_SET_MCCONF_TEMP_SETUP				= 49,
	COMM_GET_VALUES_SELECTIVE				= 50,
	COMM_GET_VALUES_SETUP_SELECTIVE			= 51,
	COMM_EXT_NRF_PRESENT					= 52,
	COMM_EXT_NRF_ESB_SET_CH_ADDR			= 53,
	COMM_EXT_NRF_ESB_SEND_DATA				= 54,
	COMM_EXT_NRF_ESB_RX_DATA				= 55,
	COMM_EXT_NRF_SET_ENABLED				= 56,
	COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP	= 57,
	COMM_DETECT_APPLY_ALL_FOC				= 58,
	COMM_JUMP_TO_BOOTLOADER_ALL_CAN			= 59,
	COMM_ERASE_NEW_APP_ALL_CAN				= 60,
	COMM_WRITE_NEW_APP_DATA_ALL_CAN			= 61,
	COMM_PING_CAN							= 62,
	COMM_APP_DISABLE_OUTPUT					= 63,
	COMM_TERMINAL_CMD_SYNC					= 64,
	COMM_GET_IMU_DATA						= 65,
	COMM_BM_CONNECT							= 66,
	COMM_BM_ERASE_FLASH_ALL					= 67,
	COMM_BM_WRITE_FLASH						= 68,
	COMM_BM_REBOOT							= 69,
	COMM_BM_DISCONNECT						= 70,
	COMM_BM_MAP_PINS_DEFAULT				= 71,
	COMM_BM_MAP_PINS_NRF5X					= 72,
	COMM_ERASE_BOOTLOADER					= 73,
	COMM_ERASE_BOOTLOADER_ALL_CAN			= 74,
	COMM_PLOT_INIT							= 75,
	COMM_PLOT_DATA							= 76,
	COMM_PLOT_ADD_GRAPH						= 77,
	COMM_PLOT_SET_GRAPH						= 78,
	COMM_GET_DECODED_BALANCE				= 79,
	COMM_BM_MEM_READ						= 80,
	COMM_WRITE_NEW_APP_DATA_LZO				= 81,
	COMM_WRITE_NEW_APP_DATA_ALL_CAN_LZO		= 82,
	COMM_BM_WRITE_FLASH_LZO					= 83,
	COMM_SET_CURRENT_REL					= 84,
	COMM_CAN_FWD_FRAME						= 85,
	COMM_SET_BATTERY_CUT					= 86,
	COMM_SET_BLE_NAME						= 87,
	COMM_SET_BLE_PIN						= 88,
	COMM_SET_CAN_MODE						= 89,
	COMM_GET_IMU_CALIBRATION				= 90,
	COMM_GET_MCCONF_TEMP					= 91,

	// Custom configuration for hardware
	COMM_GET_CUSTOM_CONFIG_XML				= 92,
	COMM_GET_CUSTOM_CONFIG					= 93,
	COMM_GET_CUSTOM_CONFIG_DEFAULT			= 94,
	COMM_SET_CUSTOM_CONFIG					= 95,

	// BMS commands
	COMM_BMS_GET_VALUES						= 96,
	COMM_BMS_SET_CHARGE_ALLOWED				= 97,
	COMM_BMS_SET_BALANCE_OVERRIDE			= 98,
	COMM_BMS_RESET_COUNTERS					= 99,
	COMM_BMS_FORCE_BALANCE					= 100,
	COMM_BMS_ZERO_CURRENT_OFFSET			= 101,

	// FW updates commands for different HW types
	COMM_JUMP_TO_BOOTLOADER_HW				= 102,
	COMM_ERASE_NEW_APP_HW					= 103,
	COMM_WRITE_NEW_APP_DATA_HW				= 104,
	COMM_ERASE_BOOTLOADER_HW				= 105,
	COMM_JUMP_TO_BOOTLOADER_ALL_CAN_HW		= 106,
	COMM_ERASE_NEW_APP_ALL_CAN_HW			= 107,
	COMM_WRITE_NEW_APP_DATA_ALL_CAN_HW		= 108,
	COMM_ERASE_BOOTLOADER_ALL_CAN_HW		= 109,

	COMM_SET_ODOMETER						= 110,

	// Power switch commands
	COMM_PSW_GET_STATUS						= 111,
	COMM_PSW_SWITCH							= 112,

	COMM_BMS_FWD_CAN_RX						= 113,
	COMM_BMS_HW_DATA						= 114,
	COMM_GET_BATTERY_CUT					= 115,
	COMM_BM_HALT_REQ						= 116,
	COMM_GET_QML_UI_HW						= 117,
	COMM_GET_QML_UI_APP						= 118,
	COMM_CUSTOM_HW_DATA						= 119,
	COMM_QMLUI_ERASE						= 120,
	COMM_QMLUI_WRITE						= 121,

	// IO Board
	COMM_IO_BOARD_GET_ALL					= 122,
	COMM_IO_BOARD_SET_PWM					= 123,
	COMM_IO_BOARD_SET_DIGITAL				= 124,

	COMM_BM_MEM_WRITE						= 125,
	COMM_BMS_BLNC_SELFTEST					= 126,
	COMM_GET_EXT_HUM_TMP					= 127,
	COMM_GET_STATS							= 128,
	COMM_RESET_STATS						= 129,

	// Lisp
	COMM_LISP_READ_CODE						= 130,
	COMM_LISP_WRITE_CODE					= 131,
	COMM_LISP_ERASE_CODE					= 132,
	COMM_LISP_SET_RUNNING					= 133,
	COMM_LISP_GET_STATS						= 134,
	COMM_LISP_PRINT							= 135,

	COMM_BMS_SET_BATT_TYPE					= 136,
	COMM_BMS_GET_BATT_TYPE					= 137,

	COMM_LISP_REPL_CMD						= 138,
	COMM_LISP_STREAM_CODE					= 139,

	COMM_FILE_LIST							= 140,
	COMM_FILE_READ							= 141,
	COMM_FILE_WRITE							= 142,
	COMM_FILE_MKDIR							= 143,
	COMM_FILE_REMOVE						= 144,

	COMM_LOG_START							= 145,
	COMM_LOG_STOP							= 146,
	COMM_LOG_CONFIG_FIELD					= 147,
	COMM_LOG_DATA_F32						= 148,

	COMM_SET_APPCONF_NO_STORE				= 149,
	COMM_GET_GNSS							= 150,

	COMM_LOG_DATA_F64						= 151,

	COMM_LISP_RMSG							= 152,

	//Placeholders for pinlock commands
	//COMM_PINLOCK1							= 153,
	//COMM_PINLOCK2							= 154,
	//COMM_PINLOCK3							= 155,

	COMM_SHUTDOWN							= 156,
} COMM_PACKET_ID;

// CAN commands
typedef enum {
	CAN_PACKET_SET_DUTY						= 0,
	CAN_PACKET_SET_CURRENT					= 1,
	CAN_PACKET_SET_CURRENT_BRAKE			= 2,
	CAN_PACKET_SET_RPM						= 3,
	CAN_PACKET_SET_POS						= 4,
	CAN_PACKET_FILL_RX_BUFFER				= 5,
	CAN_PACKET_FILL_RX_BUFFER_LONG			= 6,
	CAN_PACKET_PROCESS_RX_BUFFER			= 7,
	CAN_PACKET_PROCESS_SHORT_BUFFER			= 8,
	CAN_PACKET_STATUS						= 9,
	CAN_PACKET_SET_CURRENT_REL				= 10,
	CAN_PACKET_SET_CURRENT_BRAKE_REL		= 11,
	CAN_PACKET_SET_CURRENT_HANDBRAKE		= 12,
	CAN_PACKET_SET_CURRENT_HANDBRAKE_REL	= 13,
	CAN_PACKET_STATUS_2						= 14,
	CAN_PACKET_STATUS_3						= 15,
	CAN_PACKET_STATUS_4						= 16,
	CAN_PACKET_PING							= 17,
	CAN_PACKET_PONG							= 18,
	CAN_PACKET_DETECT_APPLY_ALL_FOC			= 19,
	CAN_PACKET_DETECT_APPLY_ALL_FOC_RES		= 20,
	CAN_PACKET_CONF_CURRENT_LIMITS			= 21,
	CAN_PACKET_CONF_STORE_CURRENT_LIMITS	= 22,
	CAN_PACKET_CONF_CURRENT_LIMITS_IN		= 23,
	CAN_PACKET_CONF_STORE_CURRENT_LIMITS_IN	= 24,
	CAN_PACKET_CONF_FOC_ERPMS				= 25,
	CAN_PACKET_CONF_STORE_FOC_ERPMS			= 26,
	CAN_PACKET_STATUS_5						= 27,
	CAN_PACKET_POLL_TS5700N8501_STATUS		= 28,
	CAN_PACKET_CONF_BATTERY_CUT				= 29,
	CAN_PACKET_CONF_STORE_BATTERY_CUT		= 30,
	CAN_PACKET_SHUTDOWN						= 31,
	CAN_PACKET_IO_BOARD_ADC_1_TO_4			= 32,
	CAN_PACKET_IO_BOARD_ADC_5_TO_8			= 33,
	CAN_PACKET_IO_BOARD_ADC_9_TO_12			= 34,
	CAN_PACKET_IO_BOARD_DIGITAL_IN			= 35,
	CAN_PACKET_IO_BOARD_SET_OUTPUT_DIGITAL	= 36,
	CAN_PACKET_IO_BOARD_SET_OUTPUT_PWM		= 37,
	CAN_PACKET_BMS_V_TOT					= 38,
	CAN_PACKET_BMS_I						= 39,
	CAN_PACKET_BMS_AH_WH					= 40,
	CAN_PACKET_BMS_V_CELL					= 41,
	CAN_PACKET_BMS_BAL						= 42,
	CAN_PACKET_BMS_TEMPS					= 43,
	CAN_PACKET_BMS_HUM						= 44,
	CAN_PACKET_BMS_SOC_SOH_TEMP_STAT		= 45,
	CAN_PACKET_PSW_STAT						= 46,
	CAN_PACKET_PSW_SWITCH					= 47,
	CAN_PACKET_BMS_HW_DATA_1				= 48,
	CAN_PACKET_BMS_HW_DATA_2				= 49,
	CAN_PACKET_BMS_HW_DATA_3				= 50,
	CAN_PACKET_BMS_HW_DATA_4				= 51,
	CAN_PACKET_BMS_HW_DATA_5				= 52,
	CAN_PACKET_BMS_AH_WH_CHG_TOTAL			= 53,
	CAN_PACKET_BMS_AH_WH_DIS_TOTAL			= 54,
	CAN_PACKET_UPDATE_PID_POS_OFFSET		= 55,
	CAN_PACKET_POLL_ROTOR_POS				= 56,
	CAN_PACKET_NOTIFY_BOOT					= 57,
	CAN_PACKET_STATUS_6						= 58,
	CAN_PACKET_GNSS_TIME					= 59,
	CAN_PACKET_GNSS_LAT						= 60,
	CAN_PACKET_GNSS_LON						= 61,
	CAN_PACKET_GNSS_ALT_SPEED_HDOP			= 62,
	CAN_PACKET_MAKE_ENUM_32_BITS = 0xFFFFFFFF,
} CAN_PACKET_ID;

typedef struct {
	double lat;
	double lon;
	float height;
	float speed;
	float hdop;
	int32_t ms_today;
	int16_t yy;
	int8_t mo;
	int8_t dd;
	systime_t last_update;
} gnss_data;

// Logged fault data
typedef struct {
	uint8_t motor;
	mc_fault_code fault;
	float current;
	float current_filtered;
	float voltage;
	float gate_driver_voltage;
	float duty;
	float rpm;
	int tacho;
	int cycles_running;
	int tim_val_samp;
	int tim_current_samp;
	int tim_top;
	int comm_step;
	float temperature;
	int drv8301_faults;
	const char *info_str;
	int info_argn;
	float info_args[2];
} fault_data;

typedef struct {
	int js_x;
	int js_y;
	int acc_x;
	int acc_y;
	int acc_z;
	bool bt_c;
	bool bt_z;
	bool rev_has_state;
	bool is_rev;
} chuck_data;

typedef struct {
	int id;
	systime_t rx_time;
	float rpm;
	float current;
	float duty;
} can_status_msg;

typedef struct {
	int id;
	systime_t rx_time;
	float amp_hours;
	float amp_hours_charged;
} can_status_msg_2;

typedef struct {
	int id;
	systime_t rx_time;
	float watt_hours;
	float watt_hours_charged;
} can_status_msg_3;

typedef struct {
	int id;
	systime_t rx_time;
	float temp_fet;
	float temp_motor;
	float current_in;
	float pid_pos_now;
} can_status_msg_4;

typedef struct {
	int id;
	systime_t rx_time;
	float v_in;
	int32_t tacho_value;
} can_status_msg_5;

typedef struct {
	int id;
	systime_t rx_time;
	float adc_1;
	float adc_2;
	float adc_3;
	float ppm;
} can_status_msg_6;

typedef struct {
	int id;
	systime_t rx_time;
	float adc_voltages[4];
} io_board_adc_values;

typedef struct {
	int id;
	systime_t rx_time;
	uint64_t inputs;
} io_board_digial_inputs;

typedef struct {
	int id;
	systime_t rx_time;
	float v_in;
	float v_out;
	float temp;
	bool is_out_on;
	bool is_pch_on;
	bool is_dsc_on;
} psw_status;

typedef struct {
	uint8_t js_x;
	uint8_t js_y;
	bool bt_c;
	bool bt_z;
	bool bt_push;
	bool rev_has_state;
	bool is_rev;
	float vbat;
} mote_state;

typedef enum {
	MOTE_PACKET_BATT_LEVEL = 0,
	MOTE_PACKET_BUTTONS,
	MOTE_PACKET_ALIVE,
	MOTE_PACKET_FILL_RX_BUFFER,
	MOTE_PACKET_FILL_RX_BUFFER_LONG,
	MOTE_PACKET_PROCESS_RX_BUFFER,
	MOTE_PACKET_PROCESS_SHORT_BUFFER,
	MOTE_PACKET_PAIRING_INFO
} MOTE_PACKET;

typedef struct {
	float v_in;
	float temp_mos;
	float temp_mos_1;
	float temp_mos_2;
	float temp_mos_3;
	float temp_motor;
    float current_motor;
    float current_in;
    float id;
    float iq;
    float rpm;
    float duty_now;
    float amp_hours;
    float amp_hours_charged;
    float watt_hours;
    float watt_hours_charged;
    int tachometer;
    int tachometer_abs;
    float position;
    mc_fault_code fault_code;
    int vesc_id;
    float vd;
    float vq;
} mc_values;

typedef enum {
	NRF_PAIR_STARTED = 0,
	NRF_PAIR_OK,
	NRF_PAIR_FAIL
} NRF_PAIR_RES;

typedef struct {
	float q0;
	float q1;
	float q2;
	float q3;
	float integralFBx;
	float integralFBy;
	float integralFBz;
	float accMagP;
	int initialUpdateDone;

	// Parameters
	float acc_confidence_decay;
	float kp;
	float ki;
	float beta;
} ATTITUDE_INFO;

// Custom EEPROM variables
typedef union {
	uint32_t as_u32;
	int32_t as_i32;
	float as_float;
} eeprom_var;

#define EEPROM_VARS_HW			32
#define EEPROM_VARS_CUSTOM		128

typedef struct {
	float ah_tot;
	float ah_charge_tot;
	float wh_tot;
	float wh_charge_tot;
	float current_tot;
	float current_in_tot;
	uint8_t num_vescs;
} setup_values;

typedef struct {
	systime_t time_start;
	double samples;
	double speed_sum;
	float max_speed;
	double power_sum;
	float max_power;
	double temp_motor_sum;
	float max_temp_motor;
	double temp_mos_sum;
	float max_temp_mos;
	double current_sum;
	float max_current;
} setup_stats;

#define BACKUP_VAR_INIT_CODE				92891934

typedef struct __attribute__((packed)) {
	uint32_t odometer_init_flag;
	uint64_t odometer; // Meters

	uint32_t runtime_init_flag;
	uint64_t runtime; // Seconds

	// HW-specific data
	uint32_t hw_config_init_flag;
	uint8_t hw_config[128];
} backup_data;

#endif /* DATATYPES_H_ */
