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
#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "lbm_memory.h"
#include "env.h"
#include "lispbm.h"

#include "commands.h"
#include "mc_interface.h"
#include "timeout.h"
#include "servo_dec.h"
#include "servo_simple.h"
#include "encoder.h"
#include "comm_can.h"
#include "bms.h"
#include "utils.h"
#include "hw.h"
#include "mcpwm_foc.h"
#include "imu.h"

#include <math.h>

// Helpers

static bool is_number_all(lbm_value *args, lbm_uint argn) {
	for (lbm_uint i = 0;i < argn;i++) {
		if (!lbm_is_number(args[i])) {
			return false;
		}
	}
	return true;
}

#define CHECK_NUMBER_ALL()			if (!is_number_all(args, argn)) {return lbm_enc_sym(SYM_EERROR);}
#define CHECK_ARGN(n)				if (argn != n) {return enc_sym(SYM_EERROR);}
#define CHECK_ARGN_NUMBER(n)		if (argn != n || !is_number_all(args, argn)) {return lbm_enc_sym(SYM_EERROR);}

// Various commands

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
	static char output[256];

	for (lbm_uint i = 0; i < argn; i ++) {
		lbm_value t = args[i];

		if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_PTR_TYPE_ARRAY) {
			lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
			switch (array->elt_type){
			case LBM_VAL_TYPE_CHAR:
				commands_printf_lisp("%s", (char*)array->data);
				break;
			default:
				return lbm_enc_sym(SYM_NIL);
				break;
			}
		} else if (lbm_type_of(t) == LBM_VAL_TYPE_CHAR) {
			if (lbm_dec_char(t) =='\n') {
				commands_printf_lisp(" ");
			} else {
				commands_printf_lisp("%c", lbm_dec_char(t));
			}
		}  else {
			lbm_print_value(output, 256, t);
			commands_printf_lisp("%s", output);
		}
	}

	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_servo(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	servo_simple_set_output(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_reset_timeout(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	timeout_reset();
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_get_ppm(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(servodec_get_servo(0));
}

static lbm_value ext_get_encoder(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(encoder_read_deg());
}

static lbm_value ext_get_vin(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_input_voltage_filtered());
}

static lbm_value ext_select_motor(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	int i = lbm_dec_as_i(args[0]);
	if (i != 0 && i != 1 && i != 2) {
		return lbm_enc_sym(SYM_EERROR);
	}
	mc_interface_select_motor_thread(i);
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_get_selected_motor(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(mc_interface_motor_now());
}

static lbm_value ext_get_bms_val(lbm_value *args, lbm_uint argn) {
	lbm_value res = lbm_enc_sym(SYM_EERROR);

	if (argn != 1 && argn != 2) {
		return lbm_enc_sym(SYM_EERROR);
	}

	char *name = lbm_dec_str(args[0]);

	if (!name) {
		return lbm_enc_sym(SYM_EERROR);
	}

	bms_values *val = bms_get_values();

	if (strcmp(name, "v_tot") == 0) {
		res = lbm_enc_F(val->v_tot);
	} else if (strcmp(name, "v_charge") == 0) {
		res = lbm_enc_F(val->v_charge);
	} else if (strcmp(name, "i_in") == 0) {
		res = lbm_enc_F(val->i_in);
	} else if (strcmp(name, "i_in_ic") == 0) {
		res = lbm_enc_F(val->i_in_ic);
	} else if (strcmp(name, "ah_cnt") == 0) {
		res = lbm_enc_F(val->ah_cnt);
	} else if (strcmp(name, "wh_cnt") == 0) {
		res = lbm_enc_F(val->wh_cnt);
	} else if (strcmp(name, "cell_num") == 0) {
		res = lbm_enc_i(val->cell_num);
	} else if (strcmp(name, "v_cell") == 0) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return lbm_enc_sym(SYM_EERROR);
		}

		int c = lbm_dec_as_i(args[1]);
		if (c < 0 || c >= val->cell_num) {
			return lbm_enc_sym(SYM_EERROR);
		}

		res = lbm_enc_F(val->v_cell[c]);
	} else if (strcmp(name, "bal_state") == 0) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return lbm_enc_sym(SYM_EERROR);
		}

		int c = lbm_dec_as_i(args[1]);
		if (c < 0 || c >= val->cell_num) {
			return lbm_enc_sym(SYM_EERROR);
		}

		res = lbm_enc_i(val->bal_state[c]);
	} else if (strcmp(name, "temp_adc_num") == 0) {
		res = lbm_enc_i(val->temp_adc_num);
	} else if (strcmp(name, "temps_adc") == 0) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return lbm_enc_sym(SYM_EERROR);
		}

		int c = lbm_dec_as_i(args[1]);
		if (c < 0 || c >= val->temp_adc_num) {
			return lbm_enc_sym(SYM_EERROR);
		}

		res = lbm_enc_F(val->temps_adc[c]);
	} else if (strcmp(name, "temp_ic") == 0) {
		res = lbm_enc_F(val->temp_ic);
	} else if (strcmp(name, "temp_hum") == 0) {
		res = lbm_enc_F(val->temp_hum);
	} else if (strcmp(name, "hum") == 0) {
		res = lbm_enc_F(val->hum);
	} else if (strcmp(name, "temp_max_cell") == 0) {
		res = lbm_enc_F(val->temp_max_cell);
	} else if (strcmp(name, "soc") == 0) {
		res = lbm_enc_F(val->soc);
	} else if (strcmp(name, "soh") == 0) {
		res = lbm_enc_F(val->soh);
	} else if (strcmp(name, "can_id") == 0) {
		res = lbm_enc_i(val->can_id);
	} else if (strcmp(name, "ah_cnt_chg_total") == 0) {
		res = lbm_enc_F(val->ah_cnt_chg_total);
	} else if (strcmp(name, "wh_cnt_chg_total") == 0) {
		res = lbm_enc_F(val->wh_cnt_chg_total);
	} else if (strcmp(name, "ah_cnt_dis_total") == 0) {
		res = lbm_enc_F(val->ah_cnt_dis_total);
	} else if (strcmp(name, "wh_cnt_dis_total") == 0) {
		res = lbm_enc_F(val->wh_cnt_dis_total);
	} else if (strcmp(name, "msg_age") == 0) {
		res = lbm_enc_F(UTILS_AGE_S(val->update_time));
	}

	return res;
}

static lbm_value ext_get_adc(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn == 0) {
		return lbm_enc_F(ADC_VOLTS(ADC_IND_EXT));
	} else if (argn == 1) {
		lbm_int channel = lbm_dec_as_i(args[0]);
		if (channel == 0) {
			return lbm_enc_F(ADC_VOLTS(ADC_IND_EXT));
		} else if (channel == 1) {
			return lbm_enc_F(ADC_VOLTS(ADC_IND_EXT2));
		} else {
			return lbm_enc_sym(SYM_EERROR);
		}
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}
}

static lbm_value ext_systime(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_I(chVTGetSystemTimeX());
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	return lbm_enc_F(UTILS_AGE_S(lbm_dec_as_u(args[0])));
}

static lbm_value ext_set_aux(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);

	int port = lbm_dec_as_u(args[0]);
	bool on = lbm_dec_as_u(args[1]);
	if (port == 1) {
		if (on) {
			AUX_ON();
		} else {
			AUX_OFF();
		}
		return lbm_enc_sym(SYM_TRUE);
	} else if (port == 2) {
		if (on) {
			AUX2_ON();
		} else {
			AUX2_OFF();
		}
		return lbm_enc_sym(SYM_TRUE);
	}

	return lbm_enc_sym(SYM_EERROR);
}

static lbm_value ext_get_imu_rpy(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float rpy[3];
	imu_get_rpy(rpy);

	lbm_value imu_data = lbm_enc_sym(SYM_NIL);
	imu_data = lbm_cons(lbm_enc_F(rpy[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(rpy[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(rpy[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_quat(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float q[4];
	imu_get_quaternions(q);

	lbm_value imu_data = lbm_enc_sym(SYM_NIL);
	imu_data = lbm_cons(lbm_enc_F(q[3]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(q[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(q[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(q[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_acc(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float acc[3];
	imu_get_accel(acc);

	lbm_value imu_data = lbm_enc_sym(SYM_NIL);
	imu_data = lbm_cons(lbm_enc_F(acc[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(acc[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(acc[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_gyro(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float gyro[3];
	imu_get_gyro(gyro);

	lbm_value imu_data = lbm_enc_sym(SYM_NIL);
	imu_data = lbm_cons(lbm_enc_F(gyro[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(gyro[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(gyro[0]), imu_data);

	return imu_data;
}

static lbm_value ext_get_imu_mag(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float mag[3];
	imu_get_mag(mag);

	lbm_value imu_data = lbm_enc_sym(SYM_NIL);
	imu_data = lbm_cons(lbm_enc_F(mag[2]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(mag[1]), imu_data);
	imu_data = lbm_cons(lbm_enc_F(mag[0]), imu_data);

	return imu_data;
}

static lbm_value ext_send_data(lbm_value *args, lbm_uint argn) {
	if (argn != 1 || lbm_type_of(args[0]) != LBM_PTR_TYPE_CONS) {
		return lbm_enc_sym(SYM_EERROR);
	}

	lbm_value curr = args[0];
	const int max_len = 20;
	uint8_t to_send[max_len];
	int ind = 0;

	while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
		lbm_value  arg = lbm_car(curr);

		if (lbm_is_number(arg)) {
			to_send[ind++] = lbm_dec_as_u(arg);
		} else {
			return lbm_enc_sym(SYM_EERROR);
		}

		if (ind == max_len) {
			break;
		}

		curr = lbm_cdr(curr);
	}

	commands_send_app_data(to_send, ind);

	return lbm_enc_sym(SYM_TRUE);
}

// Motor set commands

static lbm_value ext_set_current(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_current(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_current_rel(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_current_rel(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_duty(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_duty(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_brake(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_brake_current(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_brake_rel(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_brake_current_rel(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_handbrake(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_handbrake(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_handbrake_rel(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_handbrake_rel(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_rpm(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_pid_speed(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_set_pos(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	mc_interface_set_pid_pos(lbm_dec_as_f(args[0]));
	return lbm_enc_sym(SYM_TRUE);
}

// Motor get commands

static lbm_value ext_get_current(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_tot_current_filtered());
}

static lbm_value ext_get_current_dir(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_tot_current_directional_filtered());
}

static lbm_value ext_get_current_in(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_tot_current_in_filtered());
}

static lbm_value ext_get_duty(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_duty_cycle_now());
}

static lbm_value ext_get_rpm(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_rpm());
}

static lbm_value ext_get_temp_fet(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_temp_fet_filtered());
}

static lbm_value ext_get_temp_mot(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_temp_motor_filtered());
}

static lbm_value ext_get_speed(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_speed());
}

static lbm_value ext_get_dist(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_distance());
}

static lbm_value ext_get_batt(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mc_interface_get_battery_level(0));
}

static lbm_value ext_get_fault(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(mc_interface_get_fault());
}

// CAN-commands

static lbm_value ext_can_current(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn == 2) {
		comm_can_set_current(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]));
	} else if (argn == 3) {
		comm_can_set_current_off_delay(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]), lbm_dec_as_f(args[2]));
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}

	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_current_rel(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn == 2) {
		comm_can_set_current_rel(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]));
	} else if (argn == 3) {
		comm_can_set_current_rel_off_delay(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]), lbm_dec_as_f(args[2]));
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}

	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_duty(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_duty(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_brake(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_current_brake(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_brake_rel(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_current_brake_rel(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_rpm(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_rpm(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_pos(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_pos(lbm_dec_as_i(args[0]), lbm_dec_as_f(args[1]));
	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_get_current(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i(args[0]));
	if (stat0) {
		return lbm_enc_F(stat0->current);
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}
}

static lbm_value ext_can_get_current_dir(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i(args[0]));
	if (stat0) {
		return lbm_enc_F(stat0->current * SIGN(stat0->duty));
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}
}

static lbm_value ext_can_send(lbm_value *args, lbm_uint argn, bool is_eid) {
	if (argn != 2 ||
			!lbm_is_number(args[0])) {
		return lbm_enc_sym(SYM_EERROR);
	}

	lbm_value curr = args[1];
	uint8_t to_send[8];
	int ind = 0;

	while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
		lbm_value  arg = lbm_car(curr);

		if (lbm_is_number(arg)) {
			to_send[ind++] = lbm_dec_as_u(arg);
		} else {
			return lbm_enc_sym(SYM_EERROR);
		}

		if (ind == 8) {
			break;
		}

		curr = lbm_cdr(curr);
	}

	if (is_eid) {
		comm_can_transmit_eid(lbm_dec_as_u(args[0]), to_send, ind);
	} else {
		comm_can_transmit_sid(lbm_dec_as_u(args[0]), to_send, ind);
	}

	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_can_send_sid(lbm_value *args, lbm_uint argn) {
	return ext_can_send(args, argn, false);
}

static lbm_value ext_can_send_eid(lbm_value *args, lbm_uint argn) {
	return ext_can_send(args, argn, true);
}

// Math

static lbm_value ext_sin(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_F(sinf(lbm_dec_as_f(args[0])));
}

static lbm_value ext_cos(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_F(cosf(lbm_dec_as_f(args[0])));
}

static lbm_value ext_atan(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_F(atanf(lbm_dec_as_f(args[0])));
}

static lbm_value ext_atan2(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2)
	return lbm_enc_F(atan2f(lbm_dec_as_f(args[0]), lbm_dec_as_f(args[1])));
}

static lbm_value ext_pow(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2)
	return lbm_enc_F(powf(lbm_dec_as_f(args[0]), lbm_dec_as_f(args[1])));
}

static lbm_value ext_sqrt(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_F(sqrtf(lbm_dec_as_f(args[0])));
}

// Bit operations

/*
 * args[0]: Initial value
 * args[1]: Offset in initial value to modify
 * args[2]: Value to modify with
 * args[3]: Size in bits of value to modify with
 */
static lbm_value ext_bits_enc_int(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(4)
	uint32_t initial = lbm_dec_as_i(args[0]);
	uint32_t offset = lbm_dec_as_i(args[1]);
	uint32_t number = lbm_dec_as_i(args[2]);
	uint32_t bits = lbm_dec_as_i(args[3]);
	initial &= ~((0xFFFFFFFF >> (32 - bits)) << offset);
	initial |= (number << (32 - bits)) >> (32 - bits - offset);

	if (initial > ((1 << 27) - 1)) {
		return lbm_enc_I(initial);
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
	CHECK_ARGN_NUMBER(3)
	uint32_t val = lbm_dec_as_i(args[0]);
	uint32_t offset = lbm_dec_as_i(args[1]);
	uint32_t bits = lbm_dec_as_i(args[2]);
	val >>= offset;
	val &= 0xFFFFFFFF >> (32 - bits);

	if (val > ((1 << 27) - 1)) {
		return lbm_enc_I(val);
	} else {
		return lbm_enc_i(val);
	}
}

// Events that will be sent to lisp if a handler is registered

static volatile bool event_handler_registered = false;
static volatile bool event_can_sid_en = false;
static volatile bool event_can_eid_en = false;
static volatile bool event_data_rx_en = false;
static lbm_uint event_handler_pid;
static lbm_uint sym_signal_can_sid;
static lbm_uint sym_signal_can_eid;
static lbm_uint sym_signal_data_rx;

static lbm_value ext_enable_event(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		return lbm_enc_sym(SYM_EERROR);
	}

	if (argn == 2 && !lbm_is_number(args[1])) {
		return lbm_enc_sym(SYM_EERROR);
	}

	bool en = true;
	if (argn == 2 && !lbm_dec_as_i(args[1])) {
		en = false;
	}

	char *name = lbm_dec_str(args[0]);

	if (!name) {
		return lbm_enc_sym(SYM_EERROR);
	}

	if (strcmp(name, "event-can-sid") == 0) {
		event_can_sid_en = en;
	} else if (strcmp(name, "event-can-eid") == 0) {
		event_can_eid_en = en;
	} else if (strcmp(name, "event-data-rx") == 0) {
		event_data_rx_en = en;
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}

	return lbm_enc_sym(SYM_TRUE);
}

static lbm_value ext_register_event_handler(lbm_value *args, lbm_uint argn) {
	if (argn != 1 ||
			lbm_type_of(args[0]) != LBM_PTR_TYPE_CONS) {
		return lbm_enc_sym(SYM_EERROR);
	}

	lbm_value  arg = lbm_car(args[0]);
	if (lbm_is_number(arg)) {
		event_handler_registered = true;
		event_handler_pid = lbm_dec_i(arg);
		return lbm_enc_sym(SYM_TRUE);
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}
}

/*
 * args[0]: Motor, 1 or 2
 * args[1]: Phase, 1, 2 or 3
 * args[2]: Use raw ADC values. Optional argument.
 */
static lbm_value ext_raw_adc_current(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn != 2 && argn != 3) {
		return lbm_enc_sym(SYM_EERROR);
	}

	uint32_t motor = lbm_dec_as_i(args[0]);
	uint32_t phase = lbm_dec_as_i(args[1]);

	volatile float ofs1, ofs2, ofs3;
	mcpwm_foc_get_current_offsets(&ofs1, &ofs2, &ofs3, motor == 2);
	float scale = FAC_CURRENT;

	if (argn == 3 && lbm_dec_as_i(args[2]) != 0) {
		scale = 1.0;
		ofs1 = 0.0; ofs2 = 0.0; ofs3 = 0.0;
	}

	if (motor == 1) {
		switch(phase) {
		case 1: return lbm_enc_F(((float)GET_CURRENT1() - ofs1) * scale);
		case 2: return lbm_enc_F(((float)GET_CURRENT2() - ofs2) * scale);
		case 3: return lbm_enc_F(((float)GET_CURRENT3() - ofs3) * scale);
		default: return lbm_enc_sym(SYM_EERROR);
		}
	} else if (motor == 2) {
#ifdef HW_HAS_DUAL_MOTORS
		switch(phase) {
		case 1: return lbm_enc_F(((float)GET_CURRENT1_M2() - ofs1) * scale);
		case 2: return lbm_enc_F(((float)GET_CURRENT2_M2() - ofs2) * scale);
		case 3: return lbm_enc_F(((float)GET_CURRENT3_M2() - ofs3) * scale);
		default: return lbm_enc_sym(SYM_EERROR);
		}
#else
		return lbm_enc_sym(SYM_EERROR);
#endif
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}
}

/*
 * args[0]: Motor, 1 or 2
 * args[1]: Phase, 1, 2 or 3
 * args[2]: Use raw ADC values. Optional argument.
 */
static lbm_value ext_raw_adc_voltage(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn != 2 && argn != 3) {
		return lbm_enc_sym(SYM_EERROR);
	}

	uint32_t motor = lbm_dec_as_i(args[0]);
	uint32_t phase = lbm_dec_as_i(args[1]);

	float ofs1, ofs2, ofs3;
	mcpwm_foc_get_voltage_offsets(&ofs1, &ofs2, &ofs3, motor == 2);
	float scale = ((VIN_R1 + VIN_R2) / VIN_R2) * ADC_VOLTS_PH_FACTOR;

	if (argn == 3 && lbm_dec_as_i(args[2]) != 0) {
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
		return lbm_enc_sym(SYM_EERROR);
#endif
	} else if (motor == 1) {
		Va = (ADC_VOLTS(ADC_IND_SENS1) - ofs1) * scale;
		Vb = (ADC_VOLTS(ADC_IND_SENS2) - ofs2) * scale;
		Vc = (ADC_VOLTS(ADC_IND_SENS3) - ofs3) * scale;
	} else {
		return lbm_enc_sym(SYM_EERROR);
	}

	switch(phase) {
	case 1: return lbm_enc_F(Va);
	case 2: return lbm_enc_F(Vb);
	case 3: return lbm_enc_F(Vc);
	default: return lbm_enc_sym(SYM_EERROR);
	}
}

static lbm_value ext_raw_mod_alpha(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mcpwm_foc_get_mod_alpha_raw());
}

static lbm_value ext_raw_mod_beta(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mcpwm_foc_get_mod_beta_raw());
}

static lbm_value ext_raw_mod_alpha_measured(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mcpwm_foc_get_mod_alpha_measured());
}

static lbm_value ext_raw_mod_beta_measured(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_F(mcpwm_foc_get_mod_beta_measured());
}

static lbm_value ext_raw_hall(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn != 1 && argn != 2) {
		return lbm_enc_sym(SYM_EERROR);
	}

	int motor = lbm_dec_i(args[0]);
	int samples = mc_interface_get_configuration()->m_hall_extra_samples;

	if (argn == 2) {
		lbm_dec_i(args[1]);
	}

	if ((motor != 1 && motor != 2) || samples < 0 || samples > 20) {
		return lbm_enc_sym(SYM_EERROR);
	}

	int hall = utils_read_hall(motor == 2, samples);

	lbm_value hall_list = lbm_enc_sym(SYM_NIL);
	hall_list = lbm_cons(lbm_enc_i((hall >> 2) & 1), hall_list);
	hall_list = lbm_cons(lbm_enc_i((hall >> 1) & 1), hall_list);
	hall_list = lbm_cons(lbm_enc_i((hall >> 0) & 1), hall_list);

	return hall_list;
}

void lispif_process_can(uint32_t can_id, uint8_t *data8, int len, bool is_ext) {
	if (!event_handler_registered) {
		return;
	}

	if (!event_can_sid_en && !is_ext) {
		return;
	}

	if (!event_can_eid_en && is_ext) {
		return;
	}

	bool ok = true;

	int timeout_cnt = 1000;
	lbm_pause_eval_with_gc(100);
	while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
		chThdSleep(1);
		timeout_cnt--;
	}
	ok = timeout_cnt > 0;

	if (ok) {
		lbm_value data = lbm_enc_sym(SYM_NIL);
		for (int i = len - 1;i >= 0;i--) {
			data = lbm_cons(lbm_enc_i(data8[i]), data);
		}

		lbm_value msg_data = lbm_cons(lbm_enc_I(can_id), data);
		lbm_value msg;

		if (is_ext) {
			msg = lbm_cons(lbm_enc_sym(sym_signal_can_eid), msg_data);
		} else {
			msg = lbm_cons(lbm_enc_sym(sym_signal_can_sid), msg_data);
		}

		lbm_send_message(event_handler_pid, msg);
	}

	lbm_continue_eval();
}

void lispif_process_custom_app_data(unsigned char *data, unsigned int len) {
	if (!event_handler_registered) {
		return;
	}

	if (!event_data_rx_en) {
		return;
	}

	bool ok = true;

	int timeout_cnt = 1000;
	lbm_pause_eval_with_gc(100);
	while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
		chThdSleep(1);
		timeout_cnt--;
	}
	ok = timeout_cnt > 0;

	if (ok) {
		lbm_value bytes = lbm_enc_sym(SYM_NIL);
		for (int i = len - 1;i >= 0;i--) {
			bytes = lbm_cons(lbm_enc_i(data[i]), bytes);
		}

		lbm_value msg = lbm_cons(lbm_enc_sym(sym_signal_data_rx), bytes);

		lbm_send_message(event_handler_pid, msg);
	}

	lbm_continue_eval();
}

void lispif_disable_all_events(void) {
	event_handler_registered = false;
	event_can_sid_en = false;
	event_can_eid_en = false;
}

void lispif_load_vesc_extensions(void) {
	lbm_add_symbol_const("signal-can-sid", &sym_signal_can_sid);
	lbm_add_symbol_const("signal-can-eid", &sym_signal_can_eid);
	lbm_add_symbol_const("signal-data-rx", &sym_signal_data_rx);

	// Various commands
	lbm_add_extension("print", ext_print);
	lbm_add_extension("timeout-reset", ext_reset_timeout);
	lbm_add_extension("get-ppm", ext_get_ppm);
	lbm_add_extension("get-encoder", ext_get_encoder);
	lbm_add_extension("set-servo", ext_set_servo);
	lbm_add_extension("get-vin", ext_get_vin);
	lbm_add_extension("select-motor", ext_select_motor);
	lbm_add_extension("get-selected-motor", ext_get_selected_motor);
	lbm_add_extension("get-bms-val", ext_get_bms_val);
	lbm_add_extension("get-adc", ext_get_adc);
	lbm_add_extension("systime", ext_systime);
	lbm_add_extension("secs-since", ext_secs_since);
	lbm_add_extension("set-aux", ext_set_aux);
	lbm_add_extension("event-register-handler", ext_register_event_handler);
	lbm_add_extension("event-enable", ext_enable_event);
	lbm_add_extension("get-imu-rpy", ext_get_imu_rpy);
	lbm_add_extension("get-imu-quat", ext_get_imu_quat);
	lbm_add_extension("get-imu-acc", ext_get_imu_acc);
	lbm_add_extension("get-imu-gyro", ext_get_imu_gyro);
	lbm_add_extension("get-imu-mag", ext_get_imu_mag);
	lbm_add_extension("send-data", ext_send_data);

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

	// Motor get commands
	lbm_add_extension("get-current", ext_get_current);
	lbm_add_extension("get-current-dir", ext_get_current_dir);
	lbm_add_extension("get-current-in", ext_get_current_in);
	lbm_add_extension("get-duty", ext_get_duty);
	lbm_add_extension("get-rpm", ext_get_rpm);
	lbm_add_extension("get-temp-fet", ext_get_temp_fet);
	lbm_add_extension("get-temp-mot", ext_get_temp_mot);
	lbm_add_extension("get-speed", ext_get_speed);
	lbm_add_extension("get-dist", ext_get_dist);
	lbm_add_extension("get-batt", ext_get_batt);
	lbm_add_extension("get-fault", ext_get_fault);

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

	lbm_add_extension("can-send-sid", ext_can_send_sid);
	lbm_add_extension("can-send-eid", ext_can_send_eid);

	// Math
	lbm_add_extension("sin", ext_sin);
	lbm_add_extension("cos", ext_cos);
	lbm_add_extension("atan", ext_atan);
	lbm_add_extension("atan2", ext_atan2);
	lbm_add_extension("pow", ext_pow);
	lbm_add_extension("sqrt", ext_sqrt);

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
}
