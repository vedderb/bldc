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

static bool to_float(VALUE t, float *f) {
	bool res = false;

	if (is_number(t)) {
		*f = dec_as_f(t);
		res = true;
	}

	return res;
}

static bool get_arg_float(VALUE *args, UINT argn, float *f) {
	if (argn != 1) {
		return false;
	}
	return to_float(args[0], f);
}

#define DEC_FLOAT()	float f; if (!get_arg_float(args, argn, &f)) {return enc_sym(SYM_EERROR);};

// Various commands

static VALUE ext_print(VALUE *args, UINT argn) {
	static char output[256];

	for (UINT i = 0; i < argn; i ++) {
		VALUE t = args[i];

		if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
			array_header_t *array = (array_header_t *)car(t);
			switch (array->elt_type){
			case VAL_TYPE_CHAR:
				commands_printf("%s", (char*)array + 8);
				break;
			default:
				return enc_sym(SYM_NIL);
				break;
			}
		} else if (val_type(t) == VAL_TYPE_CHAR) {
			if (dec_char(t) =='\n') {
				commands_printf(" ");
			} else {
				commands_printf("%c", dec_char(t));
			}
		}  else {
			print_value(output, 256, t);
			commands_printf("%s", output);
		}
	}

	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_servo(VALUE *args, UINT argn) {
	DEC_FLOAT()
	servo_simple_set_output(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_reset_timeout(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	timeout_reset();
	return enc_sym(SYM_TRUE);
}

static VALUE ext_get_ppm(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(servodec_get_servo(0));
}

static VALUE ext_get_encoder(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(encoder_read_deg());
}

static VALUE ext_get_vin(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_input_voltage_filtered());
}

static VALUE ext_select_motor(VALUE *args, UINT argn) {
	DEC_FLOAT()
	int i = roundf(f);
	if (i != 0 && i != 1 && i != 2) {
		return enc_sym(SYM_EERROR);
	}
	mc_interface_select_motor_thread(i);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_get_selected_motor(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_i(mc_interface_motor_now());
}

static VALUE ext_get_bms_val(VALUE *args, UINT argn) {
	VALUE res = enc_sym(SYM_EERROR);

	if (argn != 1 && argn != 2) {
		return enc_sym(SYM_EERROR);
	}

	char *name = dec_str(args[0]);

	if (!name) {
		return enc_sym(SYM_EERROR);
	}

	bms_values *val = bms_get_values();

	if (strcmp(name, "v_tot") == 0) {
		res = enc_F(val->v_tot);
	} else if (strcmp(name, "v_charge") == 0) {
		res = enc_F(val->v_charge);
	} else if (strcmp(name, "i_in") == 0) {
		res = enc_F(val->i_in);
	} else if (strcmp(name, "i_in_ic") == 0) {
		res = enc_F(val->i_in_ic);
	} else if (strcmp(name, "ah_cnt") == 0) {
		res = enc_F(val->ah_cnt);
	} else if (strcmp(name, "wh_cnt") == 0) {
		res = enc_F(val->wh_cnt);
	} else if (strcmp(name, "cell_num") == 0) {
		res = enc_i(val->cell_num);
	} else if (strcmp(name, "v_cell") == 0) {
		float f;
		if (argn != 2 || !to_float(args[1], &f)) {
			return enc_sym(SYM_EERROR);
		}

		int c = roundf(f);
		if (c < 0 || c >= val->cell_num) {
			return enc_sym(SYM_EERROR);
		}

		res = enc_F(val->v_cell[c]);
	} else if (strcmp(name, "bal_state") == 0) {
		float f;
		if (argn != 2 || !to_float(args[1], &f)) {
			return enc_sym(SYM_EERROR);
		}

		int c = roundf(f);
		if (c < 0 || c >= val->cell_num) {
			return enc_sym(SYM_EERROR);
		}

		res = enc_i(val->bal_state[c]);
	} else if (strcmp(name, "temp_adc_num") == 0) {
		res = enc_i(val->temp_adc_num);
	} else if (strcmp(name, "temps_adc") == 0) {
		float f;
		if (argn != 2 || !to_float(args[1], &f)) {
			return enc_sym(SYM_EERROR);
		}

		int c = roundf(f);
		if (c < 0 || c >= val->temp_adc_num) {
			return enc_sym(SYM_EERROR);
		}

		res = enc_F(val->temps_adc[c]);
	} else if (strcmp(name, "temp_ic") == 0) {
		res = enc_F(val->temp_ic);
	} else if (strcmp(name, "temp_hum") == 0) {
		res = enc_F(val->temp_hum);
	} else if (strcmp(name, "hum") == 0) {
		res = enc_F(val->hum);
	} else if (strcmp(name, "temp_max_cell") == 0) {
		res = enc_F(val->temp_max_cell);
	} else if (strcmp(name, "soc") == 0) {
		res = enc_F(val->soc);
	} else if (strcmp(name, "soh") == 0) {
		res = enc_F(val->soh);
	} else if (strcmp(name, "can_id") == 0) {
		res = enc_i(val->can_id);
	} else if (strcmp(name, "ah_cnt_chg_total") == 0) {
		res = enc_F(val->ah_cnt_chg_total);
	} else if (strcmp(name, "wh_cnt_chg_total") == 0) {
		res = enc_F(val->wh_cnt_chg_total);
	} else if (strcmp(name, "ah_cnt_dis_total") == 0) {
		res = enc_F(val->ah_cnt_dis_total);
	} else if (strcmp(name, "wh_cnt_dis_total") == 0) {
		res = enc_F(val->wh_cnt_dis_total);
	} else if (strcmp(name, "msg_age") == 0) {
		res = enc_F(UTILS_AGE_S(val->update_time));
	}

	return res;
}

static VALUE ext_get_adc(VALUE *args, UINT argn) {
	if (argn == 0) {
		return enc_F(ADC_VOLTS(ADC_IND_EXT));
	} else if (argn == 1) {
		if (is_number(args[0])) {
			INT channel = dec_as_i(args[0]);
			if (channel == 0) {
				return enc_F(ADC_VOLTS(ADC_IND_EXT));
			} else if (channel == 1) {
				return enc_F(ADC_VOLTS(ADC_IND_EXT2));
			} else {
				return enc_sym(SYM_EERROR);
			}
		} else {
			return enc_sym(SYM_EERROR);
		}
	} else {
		return enc_sym(SYM_EERROR);
	}
}

// Motor set commands

static VALUE ext_set_current(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_current(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_current_rel(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_current_rel(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_duty(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_duty(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_brake(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_brake_current(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_brake_rel(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_brake_current_rel(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_handbrake(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_handbrake(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_handbrake_rel(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_handbrake_rel(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_speed(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_pid_speed(f);
	return enc_sym(SYM_TRUE);
}

static VALUE ext_set_pos(VALUE *args, UINT argn) {
	DEC_FLOAT()
	mc_interface_set_pid_pos(f);
	return enc_sym(SYM_TRUE);
}

// Motor get commands

static VALUE ext_get_current(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_tot_current_filtered());
}

static VALUE ext_get_current_dir(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_tot_current_directional_filtered());
}

static VALUE ext_get_current_in(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_tot_current_in_filtered());
}

static VALUE ext_get_duty(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_duty_cycle_now());
}

static VALUE ext_get_rpm(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_rpm());
}

static VALUE ext_get_temp_fet(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_temp_fet_filtered());
}

static VALUE ext_get_temp_mot(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_temp_motor_filtered());
}

static VALUE ext_get_speed(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_speed());
}

static VALUE ext_get_dist(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_distance_abs());
}

static VALUE ext_get_batt(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_F(mc_interface_get_battery_level(0));
}

static VALUE ext_get_fault(VALUE *args, UINT argn) {
	(void)args; (void)argn;
	return enc_i(mc_interface_get_fault());
}

void lispif_load_vesc_extensions(void) {
	// Various commands
	extensions_add("print", ext_print);
	extensions_add("timeout-reset", ext_reset_timeout);
	extensions_add("get-ppm", ext_get_ppm);
	extensions_add("get-encoder", ext_get_encoder);
	extensions_add("set-servo", ext_set_servo);
	extensions_add("get-vin", ext_get_vin);
	extensions_add("select-motor", ext_select_motor);
	extensions_add("get-selected-motor", ext_get_selected_motor);
	extensions_add("get-bms-val", ext_get_bms_val);
	extensions_add("get-adc", ext_get_adc);

	// Motor set commands
	extensions_add("set-current", ext_set_current);
	extensions_add("set-current-rel", ext_set_current_rel);
	extensions_add("set-duty", ext_set_duty);
	extensions_add("set-brake", ext_set_brake);
	extensions_add("set-brake-rel", ext_set_brake_rel);
	extensions_add("set-handbrake", ext_set_handbrake);
	extensions_add("set-handbrake-rel", ext_set_handbrake_rel);
	extensions_add("set-speed", ext_set_speed);
	extensions_add("set-pos", ext_set_pos);

	// Motor get commands
	extensions_add("get-current", ext_get_current);
	extensions_add("get-current-dir", ext_get_current_dir);
	extensions_add("get-current-in", ext_get_current_in);
	extensions_add("get-duty", ext_get_duty);
	extensions_add("get-rpm", ext_get_rpm);
	extensions_add("get-temp-fet", ext_get_temp_fet);
	extensions_add("get-temp-mot", ext_get_temp_mot);
	extensions_add("get-speed", ext_get_speed);
	extensions_add("get-dist", ext_get_dist);
	extensions_add("get-batt", ext_get_batt);
	extensions_add("get-fault", ext_get_fault);
}
