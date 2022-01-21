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
#include "extensions.h"
#include "print.h"

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
				commands_printf("%s", (char*)array + 8);
				break;
			default:
				return lbm_enc_sym(SYM_NIL);
				break;
			}
		} else if (lbm_type_of(t) == LBM_VAL_TYPE_CHAR) {
			if (lbm_dec_char(t) =='\n') {
				commands_printf(" ");
			} else {
				commands_printf("%c", lbm_dec_char(t));
			}
		}  else {
			lbm_print_value(output, 256, t);
			commands_printf("%s", output);
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
	return lbm_enc_F(mc_interface_get_distance_abs());
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

static lbm_value ext_pow(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2)
	return lbm_enc_F(powf(lbm_dec_as_f(args[0]), lbm_dec_as_f(args[1])));
}

void lispif_load_vesc_extensions(void) {
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

	// Math
	lbm_add_extension("sin", ext_sin);
	lbm_add_extension("cos", ext_cos);
	lbm_add_extension("atan", ext_atan);
	lbm_add_extension("pow", ext_pow);
}
