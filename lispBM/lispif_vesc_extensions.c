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

#include <math.h>
#include <ctype.h>
#include <stdarg.h>

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
	lbm_uint l_min_vin;
	lbm_uint l_max_vin;
	lbm_uint l_min_duty;
	lbm_uint l_max_duty;
	lbm_uint l_watt_min;
	lbm_uint l_watt_max;
	lbm_uint motor_type;
	lbm_uint foc_sensor_mode;
	lbm_uint foc_current_kp;
	lbm_uint foc_current_ki;
	lbm_uint foc_motor_l;
	lbm_uint foc_motor_ld_lq_diff;
	lbm_uint foc_motor_r;
	lbm_uint foc_motor_flux_linkage;
	lbm_uint foc_observer_gain;
	lbm_uint foc_hfi_voltage_start;
	lbm_uint foc_hfi_voltage_run;
	lbm_uint foc_hfi_voltage_max;
	lbm_uint foc_sl_erpm_hfi;
	lbm_uint m_invert_direction;
	lbm_uint m_out_aux_mode;
	lbm_uint min_speed;
	lbm_uint max_speed;
	lbm_uint controller_id;
	lbm_uint app_to_use;
	lbm_uint ppm_ctrl_type;
	lbm_uint ppm_pulse_start;
	lbm_uint ppm_pulse_end;
	lbm_uint ppm_pulse_center;
	lbm_uint ppm_ramp_time_pos;
	lbm_uint ppm_ramp_time_neg;

	// Sysinfo
	lbm_uint hw_name;
	lbm_uint fw_ver;
	lbm_uint has_phase_filters;
	lbm_uint uuid;
	lbm_uint runtime;
	lbm_uint git_branch;
	lbm_uint git_hash;
	lbm_uint compiler;

	// Rates
	lbm_uint rate_100k;
	lbm_uint rate_200k;
	lbm_uint rate_400k;
	lbm_uint rate_700k;

	// Other
	lbm_uint half_duplex;
} vesc_syms;

static vesc_syms syms_vesc = {0};
static void(*ext_callback)(void) = 0;

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
		} else if (comp == &syms_vesc.motor_type) {
			get_add_symbol("motor-type", comp);
		} else if (comp == &syms_vesc.foc_sensor_mode) {
			get_add_symbol("foc-sensor-mode", comp);
		} else if (comp == &syms_vesc.foc_current_kp) {
			get_add_symbol("foc-current-kp", comp);
		} else if (comp == &syms_vesc.foc_current_ki) {
			get_add_symbol("foc-current-ki", comp);
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
		} else if (comp == &syms_vesc.foc_sl_erpm_hfi) {
			get_add_symbol("foc-sl-erpm-hfi", comp);
		} else if (comp == &syms_vesc.m_invert_direction) {
			get_add_symbol("m-invert-direction", comp);
		} else if (comp == &syms_vesc.m_out_aux_mode) {
			get_add_symbol("m-out-aux-mode", comp);
		} else if (comp == &syms_vesc.min_speed) {
			get_add_symbol("min-speed", comp);
		} else if (comp == &syms_vesc.max_speed) {
			get_add_symbol("max-speed", comp);
		} else if (comp == &syms_vesc.controller_id) {
			get_add_symbol("controller-id", comp);
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

// Helpers

static bool is_number_all(lbm_value *args, lbm_uint argn) {
	for (lbm_uint i = 0;i < argn;i++) {
		if (!lbm_is_number(args[i])) {
			return false;
		}
	}

	return true;
}

static char *error_reason_no_number = "Argument(s) must be a number.";
static char *error_reason_argn = "Invalid number of arguments.";

static bool check_number_all(lbm_value *args, lbm_uint argn) {
	if (!is_number_all(args, argn)) {
		lbm_set_error_reason(error_reason_no_number);
		return false;
	}
	return true;
}

static bool check_argn(lbm_uint argn, lbm_uint n) {
	if (argn != n) {
		lbm_set_error_reason(error_reason_argn);
		return false;
	} else {
		return true;
	}
}

static bool check_argn_number(lbm_value *args, lbm_uint argn, lbm_uint n) {
	if (!is_number_all(args, argn)) {
		lbm_set_error_reason(error_reason_no_number);
		return false;
	} else if (argn != n) {
		lbm_set_error_reason(error_reason_argn);
		return false;
	} else {
		return true;
	}
}

#define CHECK_NUMBER_ALL() if (!check_number_all(args, argn)) {return ENC_SYM_EERROR;}
#define CHECK_ARGN(n) if (!check_argn(argn, n)) {return ENC_SYM_EERROR;}
#define CHECK_ARGN_NUMBER(n) if (!check_argn_number(args, argn, n)) {return ENC_SYM_EERROR;}

static bool gpio_get_pin(lbm_uint sym, stm32_gpio_t **port, int *pin) {
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

	return false;
}

// Various commands

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
	static char output[256];

	for (lbm_uint i = 0; i < argn; i ++) {
		lbm_value t = args[i];

		if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
			lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
			switch (array->elt_type){
			case LBM_TYPE_CHAR:
				commands_printf_lisp("%s", (char*)array->data);
				break;
			default:
				return ENC_SYM_NIL;
				break;
			}
		} else if (lbm_type_of(t) == LBM_TYPE_CHAR) {
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

	return ENC_SYM_TRUE;
}

static lbm_value ext_set_servo(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
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
	return lbm_enc_float(servodec_get_servo(0));
}

static lbm_value ext_get_ppm_age(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float((float)servodec_get_time_since_update() / 1000.0);
}

static lbm_value ext_get_encoder(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(encoder_read_deg());
}

static lbm_value ext_get_vin(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_input_voltage_filtered());
}

static lbm_value ext_select_motor(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
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

static lbm_value ext_get_bms_val(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if (argn != 1 && argn != 2) {
		return res;
	}

	if (lbm_type_of(args[0]) != LBM_TYPE_SYMBOL) {
		return res;
	}

	lbm_uint name = lbm_dec_sym(args[0]);
	bms_values *val = bms_get_values();

	if (compare_symbol(name, &syms_vesc.v_tot)) {
		res = lbm_enc_float(val->v_tot);
	} else if (compare_symbol(name, &syms_vesc.v_charge)) {
		res = lbm_enc_float(val->v_charge);
	} else if (compare_symbol(name, &syms_vesc.i_in)) {
		res = lbm_enc_float(val->i_in);
	} else if (compare_symbol(name, &syms_vesc.i_in_ic)) {
		res = lbm_enc_float(val->i_in_ic);
	} else if (compare_symbol(name, &syms_vesc.ah_cnt)) {
		res = lbm_enc_float(val->ah_cnt);
	} else if (compare_symbol(name, &syms_vesc.wh_cnt)) {
		res = lbm_enc_float(val->wh_cnt);
	} else if (compare_symbol(name, &syms_vesc.cell_num)) {
		res = lbm_enc_i(val->cell_num);
	} else if (compare_symbol(name, &syms_vesc.v_cell)) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return ENC_SYM_EERROR;
		}

		int c = lbm_dec_as_i32(args[1]);
		if (c < 0 || c >= val->cell_num) {
			return ENC_SYM_EERROR;
		}

		res = lbm_enc_float(val->v_cell[c]);
	} else if (compare_symbol(name, &syms_vesc.bal_state)) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return ENC_SYM_EERROR;
		}

		int c = lbm_dec_as_i32(args[1]);
		if (c < 0 || c >= val->cell_num) {
			return ENC_SYM_EERROR;
		}

		res = lbm_enc_i(val->bal_state[c]);
	} else if (compare_symbol(name, &syms_vesc.temp_adc_num)) {
		res = lbm_enc_i(val->temp_adc_num);
	} else if (compare_symbol(name, &syms_vesc.temps_adc)) {
		if (argn != 2 || !lbm_is_number(args[1])) {
			return ENC_SYM_EERROR;
		}

		int c = lbm_dec_as_i32(args[1]);
		if (c < 0 || c >= val->temp_adc_num) {
			return ENC_SYM_EERROR;
		}

		res = lbm_enc_float(val->temps_adc[c]);
	} else if (compare_symbol(name, &syms_vesc.temp_ic)) {
		res = lbm_enc_float(val->temp_ic);
	} else if (compare_symbol(name, &syms_vesc.temp_hum)) {
		res = lbm_enc_float(val->temp_hum);
	} else if (compare_symbol(name, &syms_vesc.hum)) {
		res = lbm_enc_float(val->hum);
	} else if (compare_symbol(name, &syms_vesc.temp_max_cell)) {
		res = lbm_enc_float(val->temp_max_cell);
	} else if (compare_symbol(name, &syms_vesc.soc)) {
		res = lbm_enc_float(val->soc);
	} else if (compare_symbol(name, &syms_vesc.soh)) {
		res = lbm_enc_float(val->soh);
	} else if (compare_symbol(name, &syms_vesc.can_id)) {
		res = lbm_enc_i(val->can_id);
	} else if (compare_symbol(name, &syms_vesc.ah_cnt_chg_total)) {
		res = lbm_enc_float(val->ah_cnt_chg_total);
	} else if (compare_symbol(name, &syms_vesc.wh_cnt_chg_total)) {
		res = lbm_enc_float(val->wh_cnt_chg_total);
	} else if (compare_symbol(name, &syms_vesc.ah_cnt_dis_total)) {
		res = lbm_enc_float(val->ah_cnt_dis_total);
	} else if (compare_symbol(name, &syms_vesc.wh_cnt_dis_total)) {
		res = lbm_enc_float(val->wh_cnt_dis_total);
	} else if (compare_symbol(name, &syms_vesc.msg_age)) {
		res = lbm_enc_float(UTILS_AGE_S(val->update_time));
	}

	return res;
}

static lbm_value ext_get_adc(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

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
		} else {
			return ENC_SYM_EERROR;
		}
	} else {
		return ENC_SYM_EERROR;
	}
}

static lbm_value ext_get_adc_decoded(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

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
	return lbm_enc_i32(chVTGetSystemTimeX());
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	return lbm_enc_float(UTILS_AGE_S(lbm_dec_as_u32(args[0])));
}

static lbm_value ext_set_aux(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);

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
	if (argn != 1 || (!lbm_is_cons(args[0]) && !lbm_is_array(args[0]))) {
		return ENC_SYM_EERROR;
	}

	lbm_value curr = args[0];
	const int max_len = 50;
	uint8_t to_send[max_len];
	uint8_t *to_send_ptr = to_send;
	int ind = 0;

	if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
		if (array->elt_type != LBM_TYPE_BYTE) {
			return ENC_SYM_EERROR;
		}

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

static lbm_value ext_get_remote_state(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;

	float gyro[3];
	imu_get_gyro_derotated(gyro);

	lbm_value state = ENC_SYM_NIL;
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
	CHECK_ARGN_NUMBER(2);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	v.as_float = lbm_dec_as_float(args[1]);
	return lbm_enc_i(conf_general_store_eeprom_var_custom(&v, addr));
}

static lbm_value ext_eeprom_read_f(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	bool res = conf_general_read_eeprom_var_custom(&v, addr);
	return res ? lbm_enc_float(v.as_float) : ENC_SYM_NIL;
}

static lbm_value ext_eeprom_store_i(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	v.as_i32 = lbm_dec_as_float(args[1]);
	return lbm_enc_i(conf_general_store_eeprom_var_custom(&v, addr));
}

static lbm_value ext_eeprom_read_i(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);

	int addr = lbm_dec_as_i32(args[0]);
	if (!check_eeprom_addr(addr)) {
		return ENC_SYM_EERROR;
	}

	eeprom_var v;
	bool res = conf_general_read_eeprom_var_custom(&v, addr);
	return res ? lbm_enc_i32(v.as_i32) : ENC_SYM_NIL;
}

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
		if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, strlen(HW_NAME) + 1)) {
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
		if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, strlen(GIT_BRANCH_NAME) + 1)) {
			lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
			strcpy((char*)arr->data, GIT_BRANCH_NAME);
			res = lbm_res;
		} else {
			res = ENC_SYM_MERROR;
		}
	} else if (compare_symbol(name, &syms_vesc.git_hash)) {
		lbm_value lbm_res;
		if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, strlen(GIT_COMMIT_HASH) + 1)) {
			lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
			strcpy((char*)arr->data, GIT_COMMIT_HASH);
			res = lbm_res;
		} else {
			res = ENC_SYM_MERROR;
		}
	} else if (compare_symbol(name, &syms_vesc.compiler)) {
		lbm_value lbm_res;
		if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, strlen(ARM_GCC_VERSION) + 1)) {
			lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
			strcpy((char*)arr->data, ARM_GCC_VERSION);
			res = lbm_res;
		} else {
			res = ENC_SYM_MERROR;
		}
	}

	return res;
}

// App set commands
static lbm_value ext_app_adc_detach(lbm_value *args, lbm_uint argn) {
	if (argn == 1){
		if(lbm_dec_as_u32(args[0]) != 0){
			return ENC_SYM_EERROR;
		}
	}else{
		CHECK_ARGN_NUMBER(2);
	}
	uint32_t mode = lbm_dec_as_u32(args[0]);
	bool detach = lbm_dec_as_char(args[1]) > 0 ? true : false;
	switch (mode){
		case 0:
			app_adc_detach_adc(false);
			app_adc_detach_buttons(false);
			break;
		case 1:
			app_adc_detach_adc(detach);
			break;
		case 2:
			app_adc_detach_buttons(detach);
			break;
		case 3:
			app_adc_detach_adc(detach);
			app_adc_detach_buttons(detach);
			break;
		default:
			return ENC_SYM_EERROR;
	}
	return ENC_SYM_TRUE;
}

static lbm_value ext_app_adc_override(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);

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


// Motor set commands

static lbm_value ext_set_current(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();
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
	CHECK_NUMBER_ALL();
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
	CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_duty(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_brake(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_brake_current(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_brake_rel(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_brake_current_rel(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_handbrake(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_handbrake(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_handbrake_rel(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_handbrake_rel(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_rpm(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_pid_speed(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_set_pos(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	timeout_reset();
	mc_interface_set_pid_pos(lbm_dec_as_float(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_foc_openloop(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	timeout_reset();
	mcpwm_foc_set_openloop(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_foc_beep(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(3);
	timeout_reset();
	mcpwm_foc_beep(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1]), lbm_dec_as_float(args[2]));
	return ENC_SYM_TRUE;
}

// Motor get commands

static lbm_value ext_get_current(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_tot_current_filtered());
}

static lbm_value ext_get_current_dir(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_tot_current_directional_filtered());
}

static lbm_value ext_get_current_in(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_tot_current_in_filtered());
}

static lbm_value ext_get_duty(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_duty_cycle_now());
}

static lbm_value ext_get_rpm(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_rpm());
}

static lbm_value ext_get_temp_fet(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_temp_fet_filtered());
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

// CAN-commands

static lbm_value ext_can_current(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

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
	CHECK_NUMBER_ALL();

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
	CHECK_ARGN_NUMBER(2);
	comm_can_set_duty(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_brake(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_current_brake(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_brake_rel(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_current_brake_rel(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_rpm(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_rpm(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_pos(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	comm_can_set_pos(lbm_dec_as_i32(args[0]), lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_can_get_current(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->current);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_current_dir(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->current * SIGN(stat0->duty));
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_current_in(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg_4 *stat4 = comm_can_get_status_msg_4_id(lbm_dec_as_i32(args[0]));
	if (stat4) {
		return lbm_enc_float((float)stat4->current_in);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_duty(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->duty);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_rpm(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg *stat0 = comm_can_get_status_msg_id(lbm_dec_as_i32(args[0]));
	if (stat0) {
		return lbm_enc_float(stat0->rpm);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_temp_fet(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg_4 *stat4 = comm_can_get_status_msg_4_id(lbm_dec_as_i32(args[0]));
	if (stat4) {
		return lbm_enc_float((float)stat4->temp_fet);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_temp_motor(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	can_status_msg_4 *stat4 = comm_can_get_status_msg_4_id(lbm_dec_as_i32(args[0]));
	if (stat4) {
		return lbm_enc_float((float)stat4->temp_motor);
	} else {
		return lbm_enc_float(0.0);
	}
}

static lbm_value ext_can_get_speed(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
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
	CHECK_ARGN_NUMBER(1);
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
	CHECK_ARGN_NUMBER(1);
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

	CHECK_NUMBER_ALL();

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

	if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
		if (array->elt_type != LBM_TYPE_BYTE) {
			return ENC_SYM_EERROR;
		}

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

// Math

static lbm_value ext_sin(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(sinf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_cos(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(cosf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_tan(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(tanf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_asin(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(asinf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_acos(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(acosf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_atan(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(atanf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_atan2(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2)
	return lbm_enc_float(atan2f(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1])));
}

static lbm_value ext_pow(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2)
	return lbm_enc_float(powf(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1])));
}

static lbm_value ext_sqrt(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(sqrtf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_log(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(logf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_log10(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1)
	return lbm_enc_float(log10f(lbm_dec_as_float(args[0])));
}

static lbm_value ext_deg2rad(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn == 1) {
		return lbm_enc_float(DEG2RAD_f(lbm_dec_as_float(args[0])));
	} else {
		lbm_value out_list = ENC_SYM_NIL;
		for (int i = (argn - 1);i >= 0;i--) {
			out_list = lbm_cons(lbm_enc_float(DEG2RAD_f(lbm_dec_as_float(args[i]))), out_list);
		}
		return out_list;
	}
}

static lbm_value ext_rad2deg(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn == 1) {
		return lbm_enc_float(RAD2DEG_f(lbm_dec_as_float(args[0])));
	} else {
		lbm_value out_list = ENC_SYM_NIL;
		for (int i = (argn - 1);i >= 0;i--) {
			out_list = lbm_cons(lbm_enc_float(RAD2DEG_f(lbm_dec_as_float(args[i]))), out_list);
		}
		return out_list;
	}
}

static lbm_value ext_vec3_rot(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();
	if (argn != 6 && argn != 7) {
		return ENC_SYM_EERROR;
	}

	float input[] = {lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1]), lbm_dec_as_float(args[2])};
	float rotation[] = {lbm_dec_as_float(args[3]), lbm_dec_as_float(args[4]), lbm_dec_as_float(args[5])};
	float output[3];

	bool reverse = false;
	if (argn == 7) {
		reverse = lbm_dec_as_i32(args[6]);
	}

	utils_rotate_vector3(input, rotation, output, reverse);

	lbm_value out_list = ENC_SYM_NIL;
	out_list = lbm_cons(lbm_enc_float(output[2]), out_list);
	out_list = lbm_cons(lbm_enc_float(output[1]), out_list);
	out_list = lbm_cons(lbm_enc_float(output[0]), out_list);

	return out_list;
}

static lbm_value ext_throttle_curve(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(4);
	return lbm_enc_float(utils_throttle_curve(
			lbm_dec_as_float(args[0]),
			lbm_dec_as_float(args[1]),
			lbm_dec_as_float(args[2]),
			lbm_dec_as_i32(args[3])));
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
	CHECK_ARGN_NUMBER(3)
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

static volatile bool event_handler_registered = false;
static volatile bool event_can_sid_en = false;
static volatile bool event_can_eid_en = false;
static volatile bool event_data_rx_en = false;
static lbm_uint event_handler_pid;
static lbm_uint sym_event_can_sid;
static lbm_uint sym_event_can_eid;
static lbm_uint sym_event_data_rx;

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
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_register_event_handler(lbm_value *args, lbm_uint argn) {
	if (argn != 1 || !lbm_is_number(args[0])) {
		return ENC_SYM_EERROR;
	}

	event_handler_pid = lbm_dec_i(args[0]);
	event_handler_registered = true;

	return ENC_SYM_TRUE;
}

/*
 * args[0]: Motor, 1 or 2
 * args[1]: Phase, 1, 2 or 3
 * args[2]: Use raw ADC values. Optional argument.
 */
static lbm_value ext_raw_adc_current(lbm_value *args, lbm_uint argn) {
	CHECK_NUMBER_ALL();

	if (argn != 2 && argn != 3) {
		return ENC_SYM_EERROR;
	}

	uint32_t motor = lbm_dec_as_i32(args[0]);
	uint32_t phase = lbm_dec_as_i32(args[1]);

	volatile float ofs1, ofs2, ofs3;
	mcpwm_foc_get_current_offsets(&ofs1, &ofs2, &ofs3, motor == 2);
	float scale = FAC_CURRENT;

	if (argn == 3 && lbm_dec_as_i32(args[2]) != 0) {
		scale = 1.0;
		ofs1 = 0.0; ofs2 = 0.0; ofs3 = 0.0;
	}

	if (motor == 1) {
		switch(phase) {
		case 1: return lbm_enc_float(((float)GET_CURRENT1() - ofs1) * scale);
		case 2: return lbm_enc_float(((float)GET_CURRENT2() - ofs2) * scale);
		case 3: return lbm_enc_float(((float)GET_CURRENT3() - ofs3) * scale);
		default: return ENC_SYM_EERROR;
		}
	} else if (motor == 2) {
#ifdef HW_HAS_DUAL_MOTORS
		switch(phase) {
		case 1: return lbm_enc_float(((float)GET_CURRENT1_M2() - ofs1) * scale);
		case 2: return lbm_enc_float(((float)GET_CURRENT2_M2() - ofs2) * scale);
		case 3: return lbm_enc_float(((float)GET_CURRENT3_M2() - ofs3) * scale);
		default: return ENC_SYM_EERROR;
		}
#else
		return ENC_SYM_EERROR;
#endif
	} else {
		return ENC_SYM_EERROR;
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
	CHECK_NUMBER_ALL();

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
	if (argn != 1 || (!lbm_is_cons(args[0]) && !lbm_is_array(args[0]))) {
		return ENC_SYM_EERROR;
	}

	if (!uart_started) {
		return ENC_SYM_EERROR;
	}

	const int max_len = 20;
	uint8_t to_send[max_len];
	uint8_t *to_send_ptr = to_send;
	int ind = 0;

	if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
		if (array->elt_type != LBM_TYPE_BYTE) {
			return ENC_SYM_EERROR;
		}

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
			lbm_type_of(args[0]) != LBM_TYPE_ARRAY || !lbm_is_number(args[1])) {
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
	if (array->elt_type != LBM_TYPE_BYTE || array->size < (num + offset)) {
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
	int sda_pin = HW_UART_RX_PIN;
	if (argn >= 2) {
		if (!lbm_is_symbol(args[1]) ||
				!gpio_get_pin(lbm_dec_sym(args[1]), &sda_gpio, &sda_pin)) {
			return ENC_SYM_EERROR;
		}
	}
	i2c_cfg.sda_gpio = sda_gpio;
	i2c_cfg.sda_pin = sda_pin;

	stm32_gpio_t *scl_gpio = HW_UART_TX_PORT;
	int scl_pin = HW_UART_TX_PIN;
	if (argn >= 3) {
		if (!lbm_is_symbol(args[2]) ||
				!gpio_get_pin(lbm_dec_sym(args[2]), &scl_gpio, &scl_pin)) {
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

	const unsigned int max_len = 20;
	uint8_t to_send[max_len];

	if (!lbm_is_number(args[0])) {
		return ENC_SYM_EERROR;
	}
	addr = lbm_dec_as_u32(args[0]);

	if (lbm_type_of(args[1]) == LBM_TYPE_ARRAY) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
		if (array->elt_type != LBM_TYPE_BYTE) {
			return ENC_SYM_EERROR;
		}

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

	if (argn >= 3 && lbm_type_of(args[2]) == LBM_TYPE_ARRAY) {
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[2]);
		if (array->elt_type != LBM_TYPE_BYTE) {
			return ENC_SYM_EERROR;
		}

		rxbuf = (uint8_t*)array->data;
		rxlen = array->size;
	}

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
	CHECK_ARGN(2);

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

	stm32_gpio_t *port; int pin;
	if (gpio_get_pin(lbm_dec_sym(args[0]), &port, &pin)) {
		palSetPadMode(port, pin, mode);
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_gpio_write(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN(2);

	if (!lbm_is_symbol(args[0]) || !lbm_is_number(args[1])) {
		return ENC_SYM_EERROR;
	}

	stm32_gpio_t *port; int pin;
	if (gpio_get_pin(lbm_dec_sym(args[0]), &port, &pin)) {
		palWritePad(port, pin, lbm_dec_as_i32(args[1]));
	} else {
		return ENC_SYM_EERROR;
	}

	return ENC_SYM_TRUE;
}

static lbm_value ext_gpio_read(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN(1);

	if (!lbm_is_symbol(args[0])) {
		return ENC_SYM_EERROR;
	}

	stm32_gpio_t *port; int pin;
	if (gpio_get_pin(lbm_dec_sym(args[0]), &port, &pin)) {
		return lbm_enc_i(palReadPad(port, pin));
	} else {
		return ENC_SYM_EERROR;
	}
}

static lbm_value ext_str_from_n(lbm_value *args, lbm_uint argn) {
	if ((argn != 1 && argn != 2) || !lbm_is_number(args[0])) {
		return ENC_SYM_EERROR;
	}

	if (argn == 2 && lbm_type_of(args[1]) != LBM_TYPE_ARRAY) {
		return ENC_SYM_EERROR;
	}

	char *format = 0;
	if (argn == 2) {
		format = lbm_dec_str(args[1]);
	}

	char buffer[100];
	size_t len = 0;

	switch (lbm_type_of(args[0])) {
	case LBM_TYPE_FLOAT:
		if (!format) {
			format = "%g";
		}
		len = snprintf(buffer, sizeof(buffer), format, (double)lbm_dec_as_float(args[0]));
		break;

	default:
		if (!format) {
			format = "%d";
		}
		len = snprintf(buffer, sizeof(buffer), format, lbm_dec_as_i32(args[0]));
		break;
	}

	if (len > sizeof(buffer)) {
		len = sizeof(buffer);
	}

	lbm_value res;
	if (lbm_create_array(&res, LBM_TYPE_CHAR, len + 1)) {
		lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
		memcpy(arr->data, buffer, len);
		((char*)(arr->data))[len] = '\0';
		return res;
	} else {
		return ENC_SYM_MERROR;
	}
}

static lbm_value ext_str_merge(lbm_value *args, lbm_uint argn) {
	int len_tot = 0;
	for (unsigned int i = 0;i < argn;i++) {
		char *str = lbm_dec_str(args[i]);
		if (str) {
			len_tot += strlen(str);
		} else {
			return ENC_SYM_EERROR;
		}
	}

	lbm_value res;
	if (lbm_create_array(&res, LBM_TYPE_CHAR, len_tot + 1)) {
		lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
		unsigned int offset = 0;
		for (unsigned int i = 0;i < argn;i++) {
			offset += sprintf((char*)arr->data + offset, "%s", lbm_dec_str(args[i]));
		}
		((char*)(arr->data))[len_tot] = '\0';
		return res;
	} else {
		return ENC_SYM_MERROR;
	}
}

static lbm_value ext_str_to_i(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		return ENC_SYM_EERROR;
	}

	char *str = lbm_dec_str(args[0]);
	if (!str) {
		return ENC_SYM_EERROR;
	}

	int base = 0;
	if (argn == 2) {
		if (!lbm_is_number(args[1])) {
			return ENC_SYM_EERROR;
		}

		base = lbm_dec_as_u32(args[1]);
	}

	return lbm_enc_i32(strtol(str, NULL, base));
}

static lbm_value ext_str_to_f(lbm_value *args, lbm_uint argn) {
	if (argn != 1) {
		return ENC_SYM_EERROR;
	}

	char *str = lbm_dec_str(args[0]);
	if (!str) {
		return ENC_SYM_EERROR;
	}

	return lbm_enc_float(strtof(str, NULL));
}

static lbm_value ext_str_part(lbm_value *args, lbm_uint argn) {
	if ((argn != 2 && argn != 3) || !lbm_is_number(args[1])) {
		return ENC_SYM_EERROR;
	}

	char *str = lbm_dec_str(args[0]);
	if (!str) {
		return ENC_SYM_EERROR;
	}

	size_t len = strlen(str);

	unsigned int start = lbm_dec_as_u32(args[1]);

	if (start >= len) {
		return ENC_SYM_EERROR;
	}

	unsigned int n = len - start;
	if (argn == 3) {
		if (!lbm_is_number(args[2])) {
			return ENC_SYM_EERROR;
		}

		n = MIN(lbm_dec_as_u32(args[2]), n);
	}

	lbm_value res;
	if (lbm_create_array(&res, LBM_TYPE_CHAR, n + 1)) {
		lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
		memcpy(arr->data, str + start, n);
		((char*)(arr->data))[n] = '\0';
		return res;
	} else {
		return ENC_SYM_MERROR;
	}
}

static lbm_value ext_str_split(lbm_value *args, lbm_uint argn) {
	if (argn != 2) {
		return ENC_SYM_EERROR;
	}

	char *str = lbm_dec_str(args[0]);
	if (!str) {
		return ENC_SYM_EERROR;
	}

	char *split = lbm_dec_str(args[1]);
	int step = 0;
	if (!split) {
		if (lbm_is_number(args[1])) {
			step = MAX(lbm_dec_as_i32(args[1]), 1);
		} else {
			return ENC_SYM_EERROR;
		}
	}

	if (step > 0) {
		lbm_value res = ENC_SYM_NIL;
		int len = strlen(str);
		for (int i = len / step;i >= 0;i--) {
			int ind_now = i * step;
			if (ind_now >= len) {
				continue;
			}

			int step_now = step;
			while ((ind_now + step_now) > len) {
				step_now--;
			}

			lbm_value tok;
			if (lbm_create_array(&tok, LBM_TYPE_CHAR, step_now + 1)) {
				lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(tok);
				memcpy(arr->data, str + ind_now, step_now);
				((char*)(arr->data))[step_now] = '\0';
				res = lbm_cons(tok, res);
			} else {
				return ENC_SYM_MERROR;
			}
		}

		return res;
	} else {
		lbm_value res = ENC_SYM_NIL;
		const char *s = str;
		while (*(s += strspn(s, split)) != '\0') {
			size_t len = strcspn(s, split);

			lbm_value tok;
			if (lbm_create_array(&tok, LBM_TYPE_CHAR, len + 1)) {
				lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(tok);
				memcpy(arr->data, s, len);
				((char*)(arr->data))[len] = '\0';
				res = lbm_cons(tok, res);
			} else {
				return ENC_SYM_MERROR;
			}

			s += len;
		}

		return lbm_list_destructive_reverse(res);
	}
}

static lbm_value ext_str_replace(lbm_value *args, lbm_uint argn) {
	if (argn != 2 && argn != 3) {
		return ENC_SYM_EERROR;
	}

	char *orig = lbm_dec_str(args[0]);
	if (!orig) {
		return ENC_SYM_TERROR;
	}

	char *rep = lbm_dec_str(args[1]);
	if (!rep) {
		return ENC_SYM_TERROR;
	}

	char *with = "";
	if (argn == 3) {
		with = lbm_dec_str(args[2]);
		if (!with) {
			return ENC_SYM_TERROR;
		}
	}

	// See https://stackoverflow.com/questions/779875/what-function-is-to-replace-a-substring-from-a-string-in-c
	char *result; // the return string
	char *ins;    // the next insert point
	char *tmp;    // varies
	int len_rep;  // length of rep (the string to remove)
	int len_with; // length of with (the string to replace rep with)
	int len_front; // distance between rep and end of last rep
	int count;    // number of replacements

	len_rep = strlen(rep);
	if (len_rep == 0) {
		return args[0]; // empty rep causes infinite loop during count
	}

	len_with = strlen(with);

	// count the number of replacements needed
	ins = orig;
	for (count = 0; (tmp = strstr(ins, rep)); ++count) {
		ins = tmp + len_rep;
	}

	size_t len_res = strlen(orig) + (len_with - len_rep) * count + 1;
	lbm_value lbm_res;
	if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, len_res)) {
		lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
		tmp = result = (char*)arr->data;
	} else {
		return ENC_SYM_MERROR;
	}

	// first time through the loop, all the variable are set correctly
	// from here on,
	//    tmp points to the end of the result string
	//    ins points to the next occurrence of rep in orig
	//    orig points to the remainder of orig after "end of rep"
	while (count--) {
		ins = strstr(orig, rep);
		len_front = ins - orig;
		tmp = strncpy(tmp, orig, len_front) + len_front;
		tmp = strcpy(tmp, with) + len_with;
		orig += len_front + len_rep; // move to next "end of rep"
	}
	strcpy(tmp, orig);

	return lbm_res;
}

static lbm_value ext_str_to_lower(lbm_value *args, lbm_uint argn) {
	if (argn != 1) {
		return ENC_SYM_EERROR;
	}

	char *orig = lbm_dec_str(args[0]);
	if (!orig) {
		return ENC_SYM_TERROR;
	}

	int len = strlen(orig);
	lbm_value lbm_res;
	if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, len + 1)) {
		lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
		for (int i = 0;i < len;i++) {
			((char*)(arr->data))[i] = tolower(orig[i]);
		}
		((char*)(arr->data))[len] = '\0';
		return lbm_res;
	} else {
		return ENC_SYM_MERROR;
	}
}

static lbm_value ext_str_to_upper(lbm_value *args, lbm_uint argn) {
	if (argn != 1) {
		return ENC_SYM_EERROR;
	}

	char *orig = lbm_dec_str(args[0]);
	if (!orig) {
		return ENC_SYM_TERROR;
	}

	int len = strlen(orig);
	lbm_value lbm_res;
	if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, len + 1)) {
		lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
		for (int i = 0;i < len;i++) {
			((char*)(arr->data))[i] = toupper(orig[i]);
		}
		((char*)(arr->data))[len] = '\0';
		return lbm_res;
	} else {
		return ENC_SYM_MERROR;
	}
}

static lbm_value ext_str_cmp(lbm_value *args, lbm_uint argn) {
	if (argn != 2) {
		return ENC_SYM_EERROR;
	}

	char *str1 = lbm_dec_str(args[0]);
	if (!str1) {
		return ENC_SYM_EERROR;
	}

	char *str2 = lbm_dec_str(args[1]);
	if (!str2) {
		return ENC_SYM_EERROR;
	}

	return lbm_enc_i(strcmp(str1, str2));
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
	} else if (compare_symbol(name, &syms_vesc.m_invert_direction)) {
		mcconf->m_invert_direction = lbm_dec_as_i32(args[1]);
		changed_mc = 1;
	} else if (compare_symbol(name, &syms_vesc.m_out_aux_mode)) {
		mcconf->m_out_aux_mode = lbm_dec_as_i32(args[1]);
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
		} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm_hfi)) {
			mcconf->foc_sl_erpm_hfi = lbm_dec_as_float(args[1]);
			changed_mc = 2;
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
	} else if (compare_symbol(name, &syms_vesc.motor_type)) {
		res = lbm_enc_i(mcconf->motor_type);
	} else if (compare_symbol(name, &syms_vesc.foc_sensor_mode)) {
		res = lbm_enc_i(mcconf->foc_sensor_mode);
	} else if (compare_symbol(name, &syms_vesc.foc_current_kp)) {
		res = lbm_enc_float(mcconf->foc_current_kp);
	} else if (compare_symbol(name, &syms_vesc.foc_current_ki)) {
		res = lbm_enc_float(mcconf->foc_current_ki);
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
	} else if (compare_symbol(name, &syms_vesc.foc_sl_erpm_hfi)) {
		res = lbm_enc_float(mcconf->foc_sl_erpm_hfi);
	} else if (compare_symbol(name, &syms_vesc.m_invert_direction)) {
		res = lbm_enc_i(mcconf->m_invert_direction);
	} else if (compare_symbol(name, &syms_vesc.m_out_aux_mode)) {
		res = lbm_enc_i(mcconf->m_out_aux_mode);
	} else if (compare_symbol(name, &syms_vesc.min_speed)) {
		res = lbm_enc_float(mcconf->l_min_erpm / speed_fact);
	} else if (compare_symbol(name, &syms_vesc.max_speed)) {
		res = lbm_enc_float(mcconf->l_max_erpm / speed_fact);
	} else if (compare_symbol(name, &syms_vesc.controller_id)) {
		res = lbm_enc_i(appconf->controller_id);
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
} detect_args;

static void detect_task(void *arg) {
	detect_args *a = (detect_args*)arg;
	int res = conf_general_detect_apply_all_foc_can(a->detect_can, a->max_power_loss,
			a->min_current_in, a->max_current_in, a->openloop_rpm, a->sl_erpm);
	lbm_unblock_ctx(a->id, lbm_enc_i(res));
}

static lbm_value ext_conf_detect_foc(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(6);
	static detect_args a;
	a.detect_can = lbm_dec_as_i32(args[0]);
	a.max_power_loss = lbm_dec_as_float(args[1]);
	a.min_current_in = lbm_dec_as_float(args[2]);
	a.max_current_in = lbm_dec_as_float(args[3]);
	a.openloop_rpm = lbm_dec_as_float(args[4]);
	a.sl_erpm = lbm_dec_as_float(args[5]);
	a.id = lbm_get_current_cid();
	worker_execute(detect_task, &a);
	lbm_block_ctx_from_extension();
	return ENC_SYM_TRUE;
}

static lbm_value ext_conf_set_pid_offset(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		lbm_set_error_reason(error_reason_argn);
		return ENC_SYM_EERROR;
	}

	CHECK_NUMBER_ALL();

	float angle = lbm_dec_as_float(args[0]);
	if (angle < -360.0 || angle > 360.0) {
		lbm_set_error_reason("Invalid angle. Range should be -360 to 360.");
		return ENC_SYM_EERROR;
	}

	bool store = false;
	if (argn == 2) {
		store = lbm_dec_as_u32(args[1]);
	}

	mc_interface_update_pid_pos_offset(angle, store);

	return ENC_SYM_TRUE;
}

typedef struct {
	float current;
	int samples;
	lbm_cid id;
} measure_res_args;

static void measure_res_task(void *arg) {
	measure_res_args *a = (measure_res_args*)arg;
	float res = mcpwm_foc_measure_resistance(a->current, a->samples, true);
	lbm_unblock_ctx(a->id, lbm_enc_float(res));
}

static lbm_value ext_conf_measure_res(lbm_value *args, lbm_uint argn) {
	if (argn != 1 && argn != 2) {
		lbm_set_error_reason(error_reason_argn);
		return ENC_SYM_EERROR;
	}

	CHECK_NUMBER_ALL();

	if (mc_interface_get_configuration()->motor_type != MOTOR_TYPE_FOC) {
		lbm_set_error_reason("Motor type must be FOC");
		return ENC_SYM_EERROR;
	}

	static measure_res_args a;
	a.current = lbm_dec_as_float(args[0]);
	a.samples = 100;
	if (argn == 2) {
		a.samples = lbm_dec_as_u32(args[1]);
	}
	a.id = lbm_get_current_cid();

	worker_execute(measure_res_task, &a);
	lbm_block_ctx_from_extension();
	return ENC_SYM_TRUE;
}

// Extra array extensions

static lbm_value ext_bufclear(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if ((argn != 1 && argn != 2 && argn != 3 && argn != 4) || !lbm_is_array(args[0])) {
		return res;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	if (array->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	uint8_t clear_byte = 0;
	if (argn >= 2) {
		if (!lbm_is_number(args[1])) {
			return res;
		}
		clear_byte = lbm_dec_as_u32(args[1]);
	}

	unsigned int start = 0;
	if (argn >= 3) {
		if (!lbm_is_number(args[2])) {
			return res;
		}
		unsigned int start_new = lbm_dec_as_u32(args[2]);
		if (start_new < array->size) {
			start = start_new;
		} else {
			return res;
		}
	}

	unsigned int len = array->size - start;
	if (argn >= 4) {
		if (!lbm_is_number(args[3])) {
			return res;
		}
		unsigned int len_new = lbm_dec_as_u32(args[3]);
		if (len_new <= len) {
			len = len_new;
		}
	}

	memset((char*)array->data + start, clear_byte, len);
	res = ENC_SYM_TRUE;

	return res;
}

static lbm_value ext_bufcpy(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if (argn != 5 || !lbm_is_array(args[0]) || !lbm_is_number(args[1])
			|| !lbm_is_array(args[2]) || !lbm_is_number(args[3]) || !lbm_is_number(args[4])) {
		return res;
	}

	lbm_array_header_t *array1 = (lbm_array_header_t *)lbm_car(args[0]);
	if (array1->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	unsigned int start1 = lbm_dec_as_u32(args[1]);

	lbm_array_header_t *array2 = (lbm_array_header_t *)lbm_car(args[2]);
	if (array2->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	unsigned int start2 = lbm_dec_as_u32(args[3]);
	unsigned int len = lbm_dec_as_u32(args[4]);

	if (start1 < array1->size && start2 < array2->size) {
		if (len > (array1->size - start1)) {
			len = (array1->size - start1);
		}
		if (len > (array2->size - start2)) {
			len = (array2->size - start2);
		}

		memcpy((char*)array1->data + start1, (char*)array2->data + start2, len);
	}

	res = ENC_SYM_TRUE;

	return res;
}

static lbm_value ext_bufset_bit(lbm_value *args, lbm_uint argn) {
	lbm_value res = ENC_SYM_EERROR;

	if (argn != 3 || !lbm_is_array(args[0]) ||
			!lbm_is_number(args[1]) || !lbm_is_number(args[2])) {
		return res;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	if (array->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	unsigned int pos = lbm_dec_as_u32(args[1]);
	unsigned int bit = lbm_dec_as_u32(args[2]) ? 1 : 0;

	unsigned int bytepos = pos / 8;
	unsigned int bitpos = pos % 8;

	if (bytepos < array->size) {
		((uint8_t*)array->data)[bytepos] &= ~(1 << bitpos);
		((uint8_t*)array->data)[bytepos] |= (bit << bitpos);
	}

	res = ENC_SYM_TRUE;

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

static lbm_value ext_me_defun(lbm_value *argsi, lbm_uint argn) {
	if (argn != 3) {
		return ENC_SYM_EERROR;
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
	CHECK_ARGN_NUMBER(1);
	int can_if = lbm_dec_as_i32(args[0]);
	uavcan_cmd_info info = canard_driver_last_rawcmd(can_if);
	lbm_value out_list = ENC_SYM_NIL;
	out_list = lbm_cons(lbm_enc_float(info.age), out_list);
	out_list = lbm_cons(lbm_enc_float(info.value), out_list);
	return out_list;
}

static lbm_value ext_uavcan_last_rpmcmd(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	int can_if = lbm_dec_as_i32(args[0]);
	uavcan_cmd_info info = canard_driver_last_rpmcmd(can_if);
	lbm_value out_list = ENC_SYM_NIL;
	out_list = lbm_cons(lbm_enc_float(info.age), out_list);
	out_list = lbm_cons(lbm_enc_float(info.value), out_list);
	return out_list;
}

static lbm_value ext_lbm_set_quota(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(1);
	uint32_t q = lbm_dec_as_u32(args[0]);

	if (q < 1) {
		return ENC_SYM_EERROR;
	}

	lbm_set_eval_step_quota(q);
	return ENC_SYM_TRUE;
}

static lbm_value ext_plot_init(lbm_value *args, lbm_uint argn) {
	if (argn != 2) {
		return ENC_SYM_EERROR;
	}

	char *namex = lbm_dec_str(args[0]);
	if (!namex) {
		return ENC_SYM_EERROR;
	}

	char *namey = lbm_dec_str(args[0]);
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
	CHECK_ARGN_NUMBER(1);
	commands_plot_set_graph(lbm_dec_as_i32(args[0]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_plot_send_points(lbm_value *args, lbm_uint argn) {
	CHECK_ARGN_NUMBER(2);
	commands_send_plot_points(
			lbm_dec_as_float(args[0]),
			lbm_dec_as_float(args[1]));
	return ENC_SYM_TRUE;
}

static lbm_value ext_empty(lbm_value *args, lbm_uint argn) {
	(void)args;(void)argn;
	return ENC_SYM_TRUE;
}

// Declare native lib extension
lbm_value ext_load_native_lib(lbm_value *args, lbm_uint argn);
lbm_value ext_unload_native_lib(lbm_value *args, lbm_uint argn);

void lispif_load_vesc_extensions(void) {
	lispif_stop_lib();

#ifdef HW_ADC_EXT_GPIO
	palSetPadMode(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN, PAL_MODE_INPUT_ANALOG);
#endif
#ifdef HW_ADC_EXT2_GPIO
	palSetPadMode(HW_ADC_EXT2_GPIO, HW_ADC_EXT2_PIN, PAL_MODE_INPUT_ANALOG);
#endif

	lbm_add_symbol_const("event-can-sid", &sym_event_can_sid);
	lbm_add_symbol_const("event-can-eid", &sym_event_can_eid);
	lbm_add_symbol_const("event-data-rx", &sym_event_data_rx);

	lbm_add_symbol_const("?01", &sym_res);
	lbm_add_symbol_const("?02", &sym_loop);
	lbm_add_symbol_const("break", &sym_break);
	lbm_add_symbol_const("?03", &sym_brk);
	lbm_add_symbol_const("?04", &sym_rst);

	memset(&syms_vesc, 0, sizeof(syms_vesc));

	// Various commands
	lbm_add_extension("print", ext_print);
	lbm_add_extension("timeout-reset", ext_reset_timeout);
	lbm_add_extension("get-ppm", ext_get_ppm);
	lbm_add_extension("get-ppm-age", ext_get_ppm_age);
	lbm_add_extension("get-encoder", ext_get_encoder);
	lbm_add_extension("set-servo", ext_set_servo);
	lbm_add_extension("get-vin", ext_get_vin);
	lbm_add_extension("select-motor", ext_select_motor);
	lbm_add_extension("get-selected-motor", ext_get_selected_motor);
	lbm_add_extension("get-bms-val", ext_get_bms_val);
	lbm_add_extension("get-adc", ext_get_adc);
	lbm_add_extension("get-adc-decoded", ext_get_adc_decoded);
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
	lbm_add_extension("get-imu-acc-derot", ext_get_imu_acc_derot);
	lbm_add_extension("get-imu-gyro-derot", ext_get_imu_gyro_derot);
	lbm_add_extension("send-data", ext_send_data);
	lbm_add_extension("get-remote-state", ext_get_remote_state);
	lbm_add_extension("eeprom-store-f", ext_eeprom_store_f);
	lbm_add_extension("eeprom-read-f", ext_eeprom_read_f);
	lbm_add_extension("eeprom-store-i", ext_eeprom_store_i);
	lbm_add_extension("eeprom-read-i", ext_eeprom_read_i);
	lbm_add_extension("sysinfo", ext_sysinfo);
	lbm_add_extension("import", ext_empty);

	// APP commands
	lbm_add_extension("app-adc-detach", ext_app_adc_detach);
	lbm_add_extension("app-adc-override", ext_app_adc_override);

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
	lbm_add_extension("get-dist-abs", ext_get_dist_abs);
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
	lbm_add_extension("canget-current-in", ext_can_get_current_in);
	lbm_add_extension("canget-duty", ext_can_get_duty);
	lbm_add_extension("canget-rpm", ext_can_get_rpm);
	lbm_add_extension("canget-temp-fet", ext_can_get_temp_fet);
	lbm_add_extension("canget-temp-motor", ext_can_get_temp_motor);
	lbm_add_extension("canget-speed", ext_can_get_speed);
	lbm_add_extension("canget-dist", ext_can_get_dist);
	lbm_add_extension("canget-ppm", ext_can_get_ppm);
	lbm_add_extension("canget-adc", ext_can_get_adc);

	lbm_add_extension("can-list-devs", ext_can_list_devs);
	lbm_add_extension("can-scan", ext_can_scan);
	lbm_add_extension("can-send-sid", ext_can_send_sid);
	lbm_add_extension("can-send-eid", ext_can_send_eid);

	// Math
	lbm_add_extension("sin", ext_sin);
	lbm_add_extension("cos", ext_cos);
	lbm_add_extension("tan", ext_tan);
	lbm_add_extension("asin", ext_asin);
	lbm_add_extension("acos", ext_acos);
	lbm_add_extension("atan", ext_atan);
	lbm_add_extension("atan2", ext_atan2);
	lbm_add_extension("pow", ext_pow);
	lbm_add_extension("sqrt", ext_sqrt);
	lbm_add_extension("log", ext_log);
	lbm_add_extension("log10", ext_log10);
	lbm_add_extension("deg2rad", ext_deg2rad);
	lbm_add_extension("rad2deg", ext_rad2deg);
	lbm_add_extension("vec3-rot", ext_vec3_rot);
	lbm_add_extension("throttle-curve", ext_throttle_curve);

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

	// String manipulation
	lbm_add_extension("str-from-n", ext_str_from_n);
	lbm_add_extension("str-merge", ext_str_merge);
	lbm_add_extension("str-to-i", ext_str_to_i);
	lbm_add_extension("str-to-f", ext_str_to_f);
	lbm_add_extension("str-part", ext_str_part);
	lbm_add_extension("str-split", ext_str_split);
	lbm_add_extension("str-replace", ext_str_replace);
	lbm_add_extension("str-to-lower", ext_str_to_lower);
	lbm_add_extension("str-to-upper", ext_str_to_upper);
	lbm_add_extension("str-cmp", ext_str_cmp);

	// Configuration
	lbm_add_extension("conf-set", ext_conf_set);
	lbm_add_extension("conf-get", ext_conf_get);
	lbm_add_extension("conf-store", ext_conf_store);
	lbm_add_extension("conf-detect-foc", ext_conf_detect_foc);
	lbm_add_extension("conf-set-pid-offset", ext_conf_set_pid_offset);
	lbm_add_extension("conf-measure-res", ext_conf_measure_res);

	// Array extensions
	lbm_array_extensions_init();
	lbm_add_extension("bufclear", ext_bufclear);
	lbm_add_extension("bufcpy", ext_bufcpy);
	lbm_add_extension("bufset-bit", ext_bufset_bit);

	// Macro expanders
	lbm_add_extension("me-defun", ext_me_defun);
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

	// Plot
	lbm_add_extension("plot-init", ext_plot_init);
	lbm_add_extension("plot-add-graph", ext_plot_add_graph);
	lbm_add_extension("plot-set-graph", ext_plot_set_graph);
	lbm_add_extension("plot-send-points", ext_plot_send_points);

	if (ext_callback) {
		ext_callback();
	}
}

void lispif_process_can(uint32_t can_id, uint8_t *data8, int len, bool is_ext) {
	if (lbm_get_eval_state() == EVAL_CPS_STATE_PAUSED) {
		return;
	}

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
		lbm_value bytes;
		if (!lbm_create_array(&bytes, LBM_TYPE_BYTE, len)) {
			lbm_continue_eval();
			return;
		}
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(bytes);
		memcpy(array->data, data8, len);

		lbm_value msg_data = lbm_cons(lbm_enc_i32(can_id), bytes);
		lbm_value msg;

		if (is_ext) {
			msg = lbm_cons(lbm_enc_sym(sym_event_can_eid), msg_data);
		} else {
			msg = lbm_cons(lbm_enc_sym(sym_event_can_sid), msg_data);
		}

		lbm_send_message(event_handler_pid, msg);
	}

	lbm_continue_eval();
}

void lispif_process_custom_app_data(unsigned char *data, unsigned int len) {
	if (lbm_get_eval_state() == EVAL_CPS_STATE_PAUSED) {
		return;
	}

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
		lbm_value bytes;
		if (!lbm_create_array(&bytes, LBM_TYPE_BYTE, len)) {
			lbm_continue_eval();
			return;
		}
		lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(bytes);
		memcpy(array->data, data, len);

		lbm_value msg = lbm_cons(lbm_enc_sym(sym_event_data_rx), bytes);

		lbm_send_message(event_handler_pid, msg);
	}

	lbm_continue_eval();
}

void lispif_set_ext_load_callback(void (*p_func)(void)) {
	ext_callback = p_func;
}

void lispif_disable_all_events(void) {
	lispif_stop_lib();
	event_handler_registered = false;
	event_can_sid_en = false;
	event_can_eid_en = false;
}
