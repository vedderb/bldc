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

#include "foc_math.h"
#include "utils_math.h"
#include <math.h>

// See http://cas.ensmp.fr/~praly/Telechargement/Journaux/2010-IEEE_TPEL-Lee-Hong-Nam-Ortega-Praly-Astolfi.pdf
void foc_observer_update(float v_alpha, float v_beta, float i_alpha, float i_beta,
		float dt, observer_state *state, float *phase, motor_all_state_t *motor) {

	mc_configuration *conf_now = motor->m_conf;

	float R = conf_now->foc_motor_r;
	float L = conf_now->foc_motor_l;
	float lambda = conf_now->foc_motor_flux_linkage;

	// Saturation compensation
	switch(conf_now->foc_sat_comp_mode) {
	case SAT_COMP_LAMBDA:
		// Here we assume that the inductance drops by the same amount as the flux linkage. I have
		// no idea if this is a valid or even a reasonable assumption.
		if (conf_now->foc_observer_type >= FOC_OBSERVER_ORTEGA_LAMBDA_COMP) {
			L = L * (state->lambda_est / lambda);
		}
		break;

	case SAT_COMP_FACTOR: {
		const float comp_fact = conf_now->foc_sat_comp * (motor->m_motor_state.i_abs_filter / conf_now->l_current_max);
		L -= L * comp_fact;
		lambda -= lambda * comp_fact;
	} break;

	case SAT_COMP_LAMBDA_AND_FACTOR: {
		if (conf_now->foc_observer_type >= FOC_OBSERVER_ORTEGA_LAMBDA_COMP) {
			L = L * (state->lambda_est / lambda);
		}
		const float comp_fact = conf_now->foc_sat_comp * (motor->m_motor_state.i_abs_filter / conf_now->l_current_max);
		L -= L * comp_fact;
	} break;

	default:
		break;
	}

	// Temperature compensation
	if (conf_now->foc_temp_comp) {
		R = motor->m_res_temp_comp;
	}

	float ld_lq_diff = conf_now->foc_motor_ld_lq_diff;
	float id = motor->m_motor_state.id;
	float iq = motor->m_motor_state.iq;

	// Adjust inductance for saliency.
	if (fabsf(id) > 0.1 || fabsf(iq) > 0.1) {
		L = L - ld_lq_diff / 2.0 + ld_lq_diff * SQ(iq) / (SQ(id) + SQ(iq));
	}

	float L_ia = L * i_alpha;
	float L_ib = L * i_beta;
	const float R_ia = R * i_alpha;
	const float R_ib = R * i_beta;
	const float gamma_half = motor->m_gamma_now * 0.5;

	switch (conf_now->foc_observer_type) {
	case FOC_OBSERVER_ORTEGA_ORIGINAL: {
		float err = SQ(lambda) - (SQ(state->x1 - L_ia) + SQ(state->x2 - L_ib));

		// Forcing this term to stay negative helps convergence according to
		//
		// http://cas.ensmp.fr/Publications/Publications/Papers/ObserverPermanentMagnet.pdf
		// and
		// https://arxiv.org/pdf/1905.00833.pdf
		if (err > 0.0) {
			err = 0.0;
		}

		float x1_dot = v_alpha - R_ia + gamma_half * (state->x1 - L_ia) * err;
		float x2_dot = v_beta - R_ib + gamma_half * (state->x2 - L_ib) * err;

		state->x1 += x1_dot * dt;
		state->x2 += x2_dot * dt;
	} break;

	case FOC_OBSERVER_MXLEMMING:
	case FOC_OBSERVER_MXLEMMING_LAMBDA_COMP:
		// LICENCE NOTE:
		// This function deviates slightly from the BSD 3 clause licence.
		// The work here is entirely original to the MESC FOC project, and not based
		// on any appnotes, or borrowed from another project. This work is free to
		// use, as granted in BSD 3 clause, with the exception that this note must
		// be included in where this code is implemented/modified to use your
		// variable names, structures containing variables or other minor
		// rearrangements in place of the original names I have chosen, and credit
		// to David Molony as the original author must be noted.

		state->x1 += (v_alpha - R_ia) * dt - L * (i_alpha - state->i_alpha_last);
		state->x2 += (v_beta - R_ib) * dt - L * (i_beta - state->i_beta_last);

		if (conf_now->foc_observer_type == FOC_OBSERVER_MXLEMMING_LAMBDA_COMP) {
			// This is essentially the flux linkage observer from
			// https://cas.mines-paristech.fr/~praly/Telechargement/Conferences/2017_IFAC_Bernard-Praly.pdf
			// with a slight modification. We use the same gain here as it is related to the Ortega-gain,
			// but we scale it down as it is not nearly as critical because the flux linkage is mostly DC.
			// When the motor starts to saturate we still want to be able to keep up though, so the gain is
			// still high enough to react with some "reasonable" speed.
			float err = SQ(state->lambda_est) - (SQ(state->x1) + SQ(state->x2));
			state->lambda_est += 0.1 * gamma_half * state->lambda_est * -err * dt;

			// Clamp the observed flux linkage (not sure if this is needed)
			utils_truncate_number(&(state->lambda_est), lambda * 0.3, lambda * 2.5);

			utils_truncate_number_abs(&(state->x1), state->lambda_est);
			utils_truncate_number_abs(&(state->x2), state->lambda_est);
		} else {
			utils_truncate_number_abs(&(state->x1), lambda);
			utils_truncate_number_abs(&(state->x2), lambda);
		}

		// Set these to 0 to allow using the same atan2-code as for Ortega
		L_ia = 0.0;
		L_ib = 0.0;
		break;

	case FOC_OBSERVER_ORTEGA_LAMBDA_COMP: {
		float err = SQ(state->lambda_est) - (SQ(state->x1 - L_ia) + SQ(state->x2 - L_ib));

		// FLux linkage observer. See:
		// https://cas.mines-paristech.fr/~praly/Telechargement/Conferences/2017_IFAC_Bernard-Praly.pdf
		state->lambda_est += 0.2 * gamma_half * state->lambda_est * -err * dt;

		// Clamp the observed flux linkage (not sure if this is needed)
		utils_truncate_number(&(state->lambda_est), lambda * 0.3, lambda * 2.5);

		if (err > 0.0) {
			err = 0.0;
		}

		float x1_dot = v_alpha - R_ia + gamma_half * (state->x1 - L_ia) * err;
		float x2_dot = v_beta - R_ib + gamma_half * (state->x2 - L_ib) * err;

		state->x1 += x1_dot * dt;
		state->x2 += x2_dot * dt;
	} break;

	default:
		break;
	}

	state->i_alpha_last = i_alpha;
	state->i_beta_last = i_beta;

	UTILS_NAN_ZERO(state->x1);
	UTILS_NAN_ZERO(state->x2);

	// Prevent the magnitude from getting too low, as that makes the angle very unstable.
	float mag = NORM2_f(state->x1, state->x2);
	if (mag < (lambda * 0.5)) {
		state->x1 *= 1.1;
		state->x2 *= 1.1;
	}

	if (phase) {
		*phase = utils_fast_atan2(state->x2 - L_ib, state->x1 - L_ia);
	}
}

void foc_pll_run(float phase, float dt, float *phase_var,
					float *speed_var, mc_configuration *conf) {
	UTILS_NAN_ZERO(*phase_var);
	float delta_theta = phase - *phase_var;
	utils_norm_angle_rad(&delta_theta);
	UTILS_NAN_ZERO(*speed_var);
	*phase_var += (*speed_var + conf->foc_pll_kp * delta_theta) * dt;
	utils_norm_angle_rad((float*)phase_var);
	*speed_var += conf->foc_pll_ki * delta_theta * dt;
}

/**
 * @brief svm Space vector modulation. Magnitude must not be larger than sqrt(3)/2, or 0.866 to avoid overmodulation.
 *        See https://github.com/vedderb/bldc/pull/372#issuecomment-962499623 for a full description.
 * @param alpha voltage
 * @param beta Park transformed and normalized voltage
 * @param PWMFullDutyCycle is the peak value of the PWM counter.
 * @param tAout PWM duty cycle phase A (0 = off all of the time, PWMFullDutyCycle = on all of the time)
 * @param tBout PWM duty cycle phase B
 * @param tCout PWM duty cycle phase C
 */
void foc_svm(float alpha, float beta, uint32_t PWMFullDutyCycle,
				uint32_t* tAout, uint32_t* tBout, uint32_t* tCout, uint32_t *svm_sector) {
	uint32_t sector;

	if (beta >= 0.0f) {
		if (alpha >= 0.0f) {
			//quadrant I
			if (ONE_BY_SQRT3 * beta > alpha) {
				sector = 2;
			} else {
				sector = 1;
			}
		} else {
			//quadrant II
			if (-ONE_BY_SQRT3 * beta > alpha) {
				sector = 3;
			} else {
				sector = 2;
			}
		}
	} else {
		if (alpha >= 0.0f) {
			//quadrant IV5
			if (-ONE_BY_SQRT3 * beta > alpha) {
				sector = 5;
			} else {
				sector = 6;
			}
		} else {
			//quadrant III
			if (ONE_BY_SQRT3 * beta > alpha) {
				sector = 4;
			} else {
				sector = 5;
			}
		}
	}

	// PWM timings
	uint32_t tA, tB, tC;

	switch (sector) {

	// sector 1-2
	case 1: {
		// Vector on-times
		uint32_t t1 = (alpha - ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;
		uint32_t t2 = (TWO_BY_SQRT3 * beta) * PWMFullDutyCycle;

		// PWM timings
		tA = (PWMFullDutyCycle + t1 + t2) / 2;
		tB = tA - t1;
		tC = tB - t2;

		break;
	}

	// sector 2-3
	case 2: {
		// Vector on-times
		uint32_t t2 = (alpha + ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;
		uint32_t t3 = (-alpha + ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;

		// PWM timings
		tB = (PWMFullDutyCycle + t2 + t3) / 2;
		tA = tB - t3;
		tC = tA - t2;

		break;
	}

	// sector 3-4
	case 3: {
		// Vector on-times
		uint32_t t3 = (TWO_BY_SQRT3 * beta) * PWMFullDutyCycle;
		uint32_t t4 = (-alpha - ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;

		// PWM timings
		tB = (PWMFullDutyCycle + t3 + t4) / 2;
		tC = tB - t3;
		tA = tC - t4;

		break;
	}

	// sector 4-5
	case 4: {
		// Vector on-times
		uint32_t t4 = (-alpha + ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;
		uint32_t t5 = (-TWO_BY_SQRT3 * beta) * PWMFullDutyCycle;

		// PWM timings
		tC = (PWMFullDutyCycle + t4 + t5) / 2;
		tB = tC - t5;
		tA = tB - t4;

		break;
	}

	// sector 5-6
	case 5: {
		// Vector on-times
		uint32_t t5 = (-alpha - ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;
		uint32_t t6 = (alpha - ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;

		// PWM timings
		tC = (PWMFullDutyCycle + t5 + t6) / 2;
		tA = tC - t5;
		tB = tA - t6;

		break;
	}

	// sector 6-1
	case 6: {
		// Vector on-times
		uint32_t t6 = (-TWO_BY_SQRT3 * beta) * PWMFullDutyCycle;
		uint32_t t1 = (alpha + ONE_BY_SQRT3 * beta) * PWMFullDutyCycle;

		// PWM timings
		tA = (PWMFullDutyCycle + t6 + t1) / 2;
		tC = tA - t1;
		tB = tC - t6;

		break;
	}
	}

	*tAout = tA;
	*tBout = tB;
	*tCout = tC;
	*svm_sector = sector;
}

void foc_run_pid_control_pos(bool index_found, float dt, motor_all_state_t *motor) {
	mc_configuration *conf_now = motor->m_conf;

	float angle_now = motor->m_pos_pid_now;
	float angle_set = motor->m_pos_pid_set;

	float p_term;
	float d_term;
	float d_term_proc;

	// PID is off. Return.
	if (motor->m_control_mode != CONTROL_MODE_POS) {
		motor->m_pos_i_term = 0;
		motor->m_pos_prev_error = 0;
		motor->m_pos_prev_proc = angle_now;
		motor->m_pos_d_filter = 0.0;
		motor->m_pos_d_filter_proc = 0.0;
		return;
	}

	// Compute parameters
	float error = utils_angle_difference(angle_set, angle_now);
	float error_sign = 1.0;

	if (conf_now->m_sensor_port_mode != SENSOR_PORT_MODE_HALL) {
		if (conf_now->foc_encoder_inverted) {
			error_sign = -1.0;
		}
	}

	error *= error_sign;

	float kp = conf_now->p_pid_kp;
	float ki = conf_now->p_pid_ki;
	float kd = conf_now->p_pid_kd;
	float kd_proc = conf_now->p_pid_kd_proc;

	if (conf_now->p_pid_gain_dec_angle > 0.1) {
		float min_error = conf_now->p_pid_gain_dec_angle / conf_now->p_pid_ang_div;
		float error_abs = fabs(error);

		if (error_abs < min_error) {
			float scale = error_abs / min_error;
			kp *= scale;
			ki *= scale;
			kd *= scale;
			kd_proc *= scale;
		}
	}

	p_term = error * kp;
	motor->m_pos_i_term += error * (ki * dt);

	// Average DT for the D term when the error does not change. This likely
	// happens at low speed when the position resolution is low and several
	// control iterations run without position updates.
	// TODO: Are there problems with this approach?
	motor->m_pos_dt_int += dt;
	if (error == motor->m_pos_prev_error) {
		d_term = 0.0;
	} else {
		d_term = (error - motor->m_pos_prev_error) * (kd / motor->m_pos_dt_int);
		motor->m_pos_dt_int = 0.0;
	}

	// Filter D
	UTILS_LP_FAST(motor->m_pos_d_filter, d_term, conf_now->p_pid_kd_filter);
	d_term = motor->m_pos_d_filter;

	// Process D term
	motor->m_pos_dt_int_proc += dt;
	if (angle_now == motor->m_pos_prev_proc) {
		d_term_proc = 0.0;
	} else {
		d_term_proc = -utils_angle_difference(angle_now, motor->m_pos_prev_proc) * error_sign * (kd_proc / motor->m_pos_dt_int_proc);
		motor->m_pos_dt_int_proc = 0.0;
	}

	// Filter D process
	UTILS_LP_FAST(motor->m_pos_d_filter_proc, d_term_proc, conf_now->p_pid_kd_filter);
	d_term_proc = motor->m_pos_d_filter_proc;

	// I-term wind-up protection
	float p_tmp = p_term;
	utils_truncate_number_abs(&p_tmp, 1.0);
	utils_truncate_number_abs((float*)&motor->m_pos_i_term, 1.0 - fabsf(p_tmp));

	// Store previous error
	motor->m_pos_prev_error = error;
	motor->m_pos_prev_proc = angle_now;

	// Calculate output
	float output = p_term + motor->m_pos_i_term + d_term + d_term_proc;
	utils_truncate_number(&output, -1.0, 1.0);

	if (conf_now->m_sensor_port_mode != SENSOR_PORT_MODE_HALL) {
		if (index_found) {
			motor->m_iq_set = output * conf_now->l_current_max * conf_now->l_current_max_scale;;
		} else {
			// Rotate the motor with 40 % power until the encoder index is found.
			motor->m_iq_set = 0.4 * conf_now->l_current_max * conf_now->l_current_max_scale;;
		}
	} else {
		motor->m_iq_set = output * conf_now->l_current_max * conf_now->l_current_max_scale;;
	}
}

void foc_run_pid_control_speed(float dt, motor_all_state_t *motor) {
	mc_configuration *conf_now = motor->m_conf;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (motor->m_control_mode != CONTROL_MODE_SPEED) {
		motor->m_speed_i_term = 0.0;
		motor->m_speed_prev_error = 0.0;
		motor->m_speed_d_filter = 0.0;
		return;
	}

	if (conf_now->s_pid_ramp_erpms_s > 0.0) {
		utils_step_towards((float*)&motor->m_speed_pid_set_rpm, motor->m_speed_command_rpm, conf_now->s_pid_ramp_erpms_s * dt);
	}

	float rpm = 0.0;
	switch (conf_now->s_pid_speed_source) {
	case S_PID_SPEED_SRC_PLL:
		rpm = RADPS2RPM_f(motor->m_pll_speed);
		break;
	case S_PID_SPEED_SRC_FAST:
		rpm = RADPS2RPM_f(motor->m_speed_est_fast);
		break;
	case S_PID_SPEED_SRC_FASTER:
		rpm = RADPS2RPM_f(motor->m_speed_est_faster);
		break;
	}

	float error = motor->m_speed_pid_set_rpm - rpm;

	// Too low RPM set. Reset state, release motor and return.
	if (fabsf(motor->m_speed_pid_set_rpm) < conf_now->s_pid_min_erpm) {
		motor->m_speed_i_term = 0.0;
		motor->m_speed_prev_error = error;
		motor->m_iq_set = 0.0;
		return;
	}

	// Compute parameters
	p_term = error * conf_now->s_pid_kp * (1.0 / 20.0);
	d_term = (error - motor->m_speed_prev_error) * (conf_now->s_pid_kd / dt) * (1.0 / 20.0);

	// Filter D
	UTILS_LP_FAST(motor->m_speed_d_filter, d_term, conf_now->s_pid_kd_filter);
	d_term = motor->m_speed_d_filter;

	// Store previous error
	motor->m_speed_prev_error = error;

	// Calculate output
	float output = p_term + motor->m_speed_i_term + d_term;
	utils_truncate_number_abs(&output, 1.0);

	// Integrator windup protection
	motor->m_speed_i_term += error * conf_now->s_pid_ki * dt * (1.0 / 20.0);
	utils_truncate_number_abs(&motor->m_speed_i_term, 1.0);

	if (conf_now->s_pid_ki < 1e-9) {
		motor->m_speed_i_term = 0.0;
	}

	// Optionally disable braking
	if (!conf_now->s_pid_allow_braking) {
		if (rpm > 20.0 && output < 0.0) {
			output = 0.0;
		}

		if (rpm < -20.0 && output > 0.0) {
			output = 0.0;
		}
	}

	motor->m_iq_set = output * conf_now->lo_current_max * conf_now->l_current_max_scale;
}

float foc_correct_encoder(float obs_angle, float enc_angle, float speed,
							 float sl_erpm, motor_all_state_t *motor) {
	float rpm_abs = fabsf(RADPS2RPM_f(speed));

	// Hysteresis 5 % of total speed
	float hyst = sl_erpm * 0.05;
	if (motor->m_using_encoder) {
		if (rpm_abs > (sl_erpm + hyst)) {
			motor->m_using_encoder = false;
		}
	} else {
		if (rpm_abs < (sl_erpm- hyst)) {
			motor->m_using_encoder = true;
		}
	}

	return motor->m_using_encoder ? enc_angle : obs_angle;
}

float foc_correct_hall(float angle, float dt, motor_all_state_t *motor, int hall_val) {
	mc_configuration *conf_now = motor->m_conf;
	motor->m_hall_dt_diff_now += dt;

	float rad_per_sec = motor->m_speed_est_fast_corrected;
	float rpm_abs = fabsf(RADPS2RPM_f(motor->m_pll_speed));
	float rpm_abs_hall = fabsf(RADPS2RPM_f(rad_per_sec));

	motor->m_using_hall = rpm_abs < conf_now->foc_sl_erpm;
	float angle_old = angle;

	int ang_hall_int = conf_now->foc_hall_table[hall_val];

	// Only override the observer if the hall sensor value is valid.
	if (ang_hall_int < 201) {
		// Scale to the circle and convert to radians
		float ang_hall_now = ((float)ang_hall_int / 200.0) * 2.0 * M_PI;

		if (motor->m_ang_hall_int_prev < 0) {
			// Previous angle not valid
			motor->m_ang_hall_int_prev = ang_hall_int;
			motor->m_ang_hall = ang_hall_now;
		} else if (ang_hall_int != motor->m_ang_hall_int_prev) {
			int diff = ang_hall_int - motor->m_ang_hall_int_prev;
			if (diff > 100) {
				diff -= 200;
			} else if (diff < -100) {
				diff += 200;
			}

			motor->m_hall_dt_diff_last = motor->m_hall_dt_diff_now;
			motor->m_hall_dt_diff_now = 0.0;

			// A transition was just made. The angle is in the middle of the new and old angle.
			int ang_avg = motor->m_ang_hall_int_prev + diff / 2;
			ang_avg %= 200;

			// Scale to the circle and convert to radians
			motor->m_ang_hall = ((float)ang_avg / 200.0) * 2.0 * M_PI;
		}

		motor->m_ang_hall_int_prev = ang_hall_int;

		if (RADPS2RPM_f((M_PI / 3.0) / fmaxf(motor->m_hall_dt_diff_now, motor->m_hall_dt_diff_last))
				< conf_now->foc_hall_interp_erpm) {
			// Don't interpolate on very low speed, just use the closest hall sensor. The reason is that we might
			// get stuck at 60 degrees off if a direction change happens between two steps.
			motor->m_ang_hall = ang_hall_now;
		} else {
			// Interpolate
			float diff = utils_angle_difference_rad(motor->m_ang_hall, ang_hall_now);
			if (fabsf(diff) < ((2.0 * M_PI) / 12.0) || SIGN(diff) != SIGN(rad_per_sec)) {
				// Do interpolation
				motor->m_ang_hall += rad_per_sec * dt;
			} else {
				// We are too far away with the interpolation
				motor->m_ang_hall -= diff * 0.01;
			}
		}

		// Limit hall sensor rate of change. This will reduce current spikes in the current controllers when the angle estimation
		// changes fast.
		float angle_step = (fmaxf(rpm_abs_hall, conf_now->foc_hall_interp_erpm) / 60.0) * 2.0 * M_PI * dt * 1.4;
		float angle_diff = utils_angle_difference_rad(motor->m_ang_hall, motor->m_ang_hall_rate_limited);
		if (fabsf(angle_diff) < angle_step) {
			motor->m_ang_hall_rate_limited = motor->m_ang_hall;
		} else {
			motor->m_ang_hall_rate_limited += angle_step * SIGN(angle_diff);
		}

		utils_norm_angle_rad((float*)&motor->m_ang_hall_rate_limited);
		utils_norm_angle_rad((float*)&motor->m_ang_hall);

		if (motor->m_using_hall) {
			angle = motor->m_ang_hall_rate_limited;
		}
	} else {
		// Invalid hall reading. Don't update angle.
		motor->m_ang_hall_int_prev = -1;

		// Also allow open loop in order to behave like normal sensorless
		// operation. Then the motor works even if the hall sensor cable
		// gets disconnected (when the sensor spacing is 120 degrees).
		if (motor->m_phase_observer_override && motor->m_state == MC_STATE_RUNNING) {
			angle = motor->m_phase_now_observer_override;
		}
	}

	// Map output angle between hall angle and observer angle in transition region to make
	// a smooth transition.
	if (angle_old != angle) {
		float weight_hall = utils_map(rpm_abs, conf_now->foc_sl_erpm_start, conf_now->foc_sl_erpm, 1.0, 0.0);
		utils_truncate_number(&weight_hall, 0.0, 1.0);
		angle = utils_interpolate_angles_rad(angle, angle_old, weight_hall);
	}

	return angle;
}

void foc_run_fw(motor_all_state_t *motor, float dt) {
	if (motor->m_conf->foc_fw_current_max < fmaxf(motor->m_conf->cc_min_current, 0.001)) {
		return;
	}

	// Field Weakening
	// FW is used in the current and speed control modes. If a different mode is used
	// this code also runs if field weakening was active before. This allows
	// changing control mode even while in field weakening.
	if (motor->m_state == MC_STATE_RUNNING &&
			(motor->m_control_mode == CONTROL_MODE_CURRENT ||
					motor->m_control_mode == CONTROL_MODE_CURRENT_BRAKE ||
					motor->m_control_mode == CONTROL_MODE_SPEED ||
					motor->m_i_fw_set > motor->m_conf->cc_min_current)) {
		float fw_current_now = 0.0;
		float duty_abs = motor->m_duty_abs_filtered;

		if (motor->m_conf->foc_fw_duty_start < 0.99 &&
				duty_abs > motor->m_conf->foc_fw_duty_start * motor->m_conf->l_max_duty) {
			fw_current_now = utils_map(duty_abs,
					motor->m_conf->foc_fw_duty_start * motor->m_conf->l_max_duty,
					motor->m_conf->l_max_duty,
					0.0, motor->m_conf->foc_fw_current_max);

			// m_current_off_delay is used to not stop the modulation too soon after leaving FW. If axis decoupling
			// is not working properly an oscillation can occur on the modulation when changing the current
			// fast, which can make the estimated duty cycle drop below the FW threshold long enough to stop
			// modulation. When that happens the body diodes in the MOSFETs can see a lot of current and unexpected
			// braking happens. Therefore the modulation is left on for some time after leaving FW to give the
			// oscillation a chance to decay while the MOSFETs are still driven.
			motor->m_current_off_delay = 1.0;
		}

		if (motor->m_conf->foc_fw_ramp_time < dt) {
			motor->m_i_fw_set = fw_current_now;
		} else {
			utils_step_towards((float*)&motor->m_i_fw_set, fw_current_now,
					(dt / motor->m_conf->foc_fw_ramp_time) * motor->m_conf->foc_fw_current_max);
		}
	}
}

void foc_hfi_adjust_angle(float ang_err, motor_all_state_t *motor, float dt) {
	mc_configuration *conf = motor->m_conf;
	// TODO: Check if ratio between these is sane or introduce separate gains
	const float gain_int = 4000.0 * conf->foc_hfi_gain;
	const float gain_int2 = 10.0 * conf->foc_hfi_gain;
	motor->m_hfi.double_integrator += ang_err * gain_int2;
	utils_truncate_number_abs(&motor->m_hfi.double_integrator, fabsf(motor->m_speed_est_fast));
	motor->m_hfi.angle -= dt * (gain_int * ang_err + motor->m_hfi.double_integrator);
	utils_norm_angle_rad((float*)&motor->m_hfi.angle);
	motor->m_hfi.ready = true;
}

void foc_precalc_values(motor_all_state_t *motor) {
	const mc_configuration *conf_now = motor->m_conf;
	motor->p_lq = conf_now->foc_motor_l + conf_now->foc_motor_ld_lq_diff * 0.5;
	motor->p_ld = conf_now->foc_motor_l - conf_now->foc_motor_ld_lq_diff * 0.5;
	motor->p_inv_ld_lq = (1.0 / motor->p_lq - 1.0 / motor->p_ld);
	motor->p_v2_v3_inv_avg_half = (0.5 / motor->p_lq + 0.5 / motor->p_ld) * 0.9; // With the 0.9 we undo the adjustment from the detection
	motor->m_observer_state.lambda_est = conf_now->foc_motor_flux_linkage;
}
