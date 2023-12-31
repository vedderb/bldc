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

#ifndef FOC_MATH_H_
#define FOC_MATH_H_

#include "datatypes.h"

// Types
typedef struct {
	float va;
	float vb;
	float vc;
	float v_mag_filter;
	float mod_alpha_filter;
	float mod_beta_filter;
	float mod_alpha_measured;
	float mod_beta_measured;
	float mod_alpha_raw;
	float mod_beta_raw;
	float id_target;
	float iq_target;
	float max_duty;
	float duty_now;
	float phase;
	float phase_cos;
	float phase_sin;
	float i_alpha;
	float i_beta;
	float i_abs;
	float i_abs_filter;
	float i_bus;
	float v_bus;
	float v_alpha;
	float v_beta;
	float mod_d;
	float mod_q;
	float mod_q_filter;
	float id;
	float iq;
	float id_filter;
	float iq_filter;
	float vd;
	float vq;
	float vd_int;
	float vq_int;
	uint32_t svm_sector;
	bool is_using_phase_filters;
} motor_state_t;

typedef struct {
	int sample_num;
	float avg_current_tot;
	float avg_voltage_tot;
} mc_sample_t;

typedef struct {
	void(*fft_bin0_func)(float*, float*, float*);
	void(*fft_bin1_func)(float*, float*, float*);
	void(*fft_bin2_func)(float*, float*, float*);

	int samples;
	int table_fact;
	float buffer[32];
	float buffer_current[32];
	bool ready;
	int ind;
	bool is_samp_n;
	float sign_last_sample;
	float cos_last, sin_last;
	float prev_sample;
	float angle;
	float double_integrator;
	int est_done_cnt;
	float observer_zero_time;
	int flip_cnt;
} hfi_state_t;

typedef struct {
	float x1;
	float x2;
	float lambda_est;
	float i_alpha_last;
	float i_beta_last;
} observer_state;

#define MC_AUDIO_CHANNELS	4

typedef enum {
	MC_AUDIO_OFF = 0,
	MC_AUDIO_TABLE,
	MC_AUDIO_SAMPLED,
} mc_audio_mode;

typedef struct {
	mc_audio_mode mode;

	const float *table[MC_AUDIO_CHANNELS];
	int table_len[MC_AUDIO_CHANNELS];
	float table_voltage[MC_AUDIO_CHANNELS];
	float table_freq[MC_AUDIO_CHANNELS];
	float table_pos[MC_AUDIO_CHANNELS];

	// Double-buffered sampled audio
	const int8_t *sample_table[2];
	int sample_table_len[2];
	bool sample_table_filled[2];
	int sample_table_now;
	float sample_freq;
	float sample_pos;
	float sample_voltage;
} mc_audio_state;

typedef struct {
	mc_configuration *m_conf;
	mc_state m_state;
	mc_control_mode m_control_mode;
	motor_state_t m_motor_state;
	float m_curr_unbalance;
	float m_currents_adc[3];
	bool m_phase_override;
	float m_phase_now_override;
	float m_duty_cycle_set;
	float m_id_set;
	float m_iq_set;
	float m_i_fw_set;
	float m_current_off_delay;
	float m_openloop_speed;
	float m_openloop_phase;
	bool m_output_on;
	float m_pos_pid_set;
	float m_speed_pid_set_rpm;
	float m_speed_command_rpm;
	float m_phase_now_observer;
	float m_phase_now_observer_override;
	float m_observer_x1_override;
	float m_observer_x2_override;
	bool m_phase_observer_override;
	float m_phase_now_encoder;
	float m_phase_now_encoder_no_index;
	observer_state m_observer_state;
	float m_pll_phase;
	float m_pll_speed;
	float m_speed_est_fast;
	float m_speed_est_fast_corrected; // Same as m_speed_est_fast, but always based on the corrected position
	float m_speed_est_faster;
	mc_sample_t m_samples;
	int m_tachometer;
	int m_tachometer_abs;
	float m_pos_pid_now;
	float m_gamma_now;
	bool m_using_encoder;
	int m_duty1_next, m_duty2_next, m_duty3_next;
	bool m_duty_next_set;
	float m_i_alpha_sample_next;
	float m_i_beta_sample_next;
	float m_i_alpha_sample_with_offset;
	float m_i_beta_sample_with_offset;
	float m_i_alpha_beta_has_offset;
	hfi_state_t m_hfi;
	int m_hfi_plot_en;
	float m_hfi_plot_sample;

	// Audio Modulation
	mc_audio_state m_audio;

	// For braking
	float m_br_speed_before;
	float m_br_vq_before;
	int m_br_no_duty_samples;

	float m_duty_abs_filtered;
	float m_duty_filtered;
	bool m_was_control_duty;
	float m_duty_i_term;
	bool duty_was_pi;
	float duty_pi_duty_last;
	float m_openloop_angle;
	float m_x1_prev;
	float m_x2_prev;
	float m_phase_before_speed_est;
	float m_phase_before_speed_est_corrected;
	int m_tacho_step_last;
	float m_pid_div_angle_last;
	float m_pid_div_angle_accumulator;
	float m_min_rpm_hyst_timer;
	float m_min_rpm_timer;
	bool m_cc_was_hfi;
	float m_pos_i_term;
	float m_pos_prev_error;
	float m_pos_dt_int;
	float m_pos_prev_proc;
	float m_pos_dt_int_proc;
	float m_pos_d_filter;
	float m_pos_d_filter_proc;
	float m_speed_i_term;
	float m_speed_prev_error;
	float m_speed_d_filter;
	int m_ang_hall_int_prev;
	bool m_using_hall;
	float m_ang_hall;
	float m_ang_hall_rate_limited;
	float m_hall_dt_diff_last;
	float m_hall_dt_diff_now;
	bool m_motor_released;

	// Resistance observer
	float m_res_est;
	float m_r_est_state;

	// Temperature-compensated parameters
	float m_res_temp_comp;
	float m_current_ki_temp_comp;

	// Pre-calculated values
	float p_lq;
	float p_ld;
	float p_inv_ld_lq; // (1.0/lq - 1.0/ld)
	float p_v2_v3_inv_avg_half; // (0.5/ld + 0.5/lq)
} motor_all_state_t;

// Functions
void foc_observer_update(float v_alpha, float v_beta, float i_alpha, float i_beta,
		float dt, observer_state *state, float *phase, motor_all_state_t *motor);
void foc_pll_run(float phase, float dt, float *phase_var,
		float *speed_var, mc_configuration *conf);
void foc_svm(float alpha, float beta, uint32_t PWMFullDutyCycle,
		uint32_t* tAout, uint32_t* tBout, uint32_t* tCout, uint32_t *svm_sector);
void foc_run_pid_control_pos(bool index_found, float dt, motor_all_state_t *motor);
void foc_run_pid_control_speed(float dt, motor_all_state_t *motor);
float foc_correct_encoder(float obs_angle, float enc_angle, float speed, float sl_erpm, motor_all_state_t *motor);
float foc_correct_hall(float angle, float dt, motor_all_state_t *motor, int hall_val);
void foc_run_fw(motor_all_state_t *motor, float dt);
void foc_hfi_adjust_angle(float ang_err, motor_all_state_t *motor, float dt);
void foc_precalc_values(motor_all_state_t *motor);

#endif /* FOC_MATH_H_ */
