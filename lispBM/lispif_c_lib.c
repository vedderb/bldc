/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#include "ch.h"
#include "hal.h"
#include "hw.h"
#include "packet.h"
#include "mc_interface.h"
#include "commands.h"
#include "comm_can.h"
#include "lispif.h"
#include "lispbm.h"
#include "utils.h"
#include "c_libs/vesc_c_if.h"
#include "worker.h"
#include "app.h"
#include "mempools.h"
#include "imu.h"
#include "terminal.h"
#include "timeout.h"
#include "conf_custom.h"

// Function prototypes otherwise missing
void packet_init(void (*s_func)(unsigned char *data, unsigned int len),
		void (*p_func)(unsigned char *data, unsigned int len), PACKET_STATE_t *state);
void packet_reset(PACKET_STATE_t *state);
void packet_process_byte(uint8_t rx_data, PACKET_STATE_t *state);
void packet_send_packet(unsigned char *data, unsigned int len, PACKET_STATE_t *state);


typedef struct {
	char *name;
	void *arg;
	void (*func)(void*);
	void *w_mem;
} lib_thd_info;

#define LIB_NUM_MAX		10
static lib_info loaded_libs[LIB_NUM_MAX] = {0};

static bool lib_init_done = false;

__attribute__((section(".libif"))) static volatile union {
	vesc_c_if cif;
	char pad[2048];
} cif;

static void lib_sleep_ms(uint32_t ms) {
	chThdSleepMilliseconds(ms);
}

static void lib_sleep_us(uint32_t us) {
	chThdSleepMicroseconds(us);
}

static float lib_system_time(void) {
	return UTILS_AGE_S(0);
}

static void* lib_malloc(size_t size) {
	lbm_uint alloc_size;
	if (size % sizeof(lbm_uint) == 0) {
		alloc_size = size / (sizeof(lbm_uint));
	} else {
		alloc_size = (size / (sizeof(lbm_uint))) + 1;
	}

	return lbm_memory_allocate(alloc_size);
}

static void lib_free(void *ptr) {
	lbm_memory_free(ptr);
}

static THD_FUNCTION(lib_thd, arg) {
	lib_thd_info *t = (lib_thd_info*)arg;
	chRegSetThreadName(t->name);
	t->func(t->arg);
	lib_free(t->w_mem);
	lib_free(t);
}

static lib_thread lib_spawn(void (*func)(void*), size_t stack_size, char *name, void *arg) {
	if (!utils_is_func_valid(func)) {
		commands_printf_lisp("Invalid function address. Make sure that the function is static.");
		return 0;
	}

	void *mem = lib_malloc(stack_size);

	if (mem) {
		lib_thd_info *info = lib_malloc(sizeof(lib_thd_info));

		if (info) {
			info->arg = arg;
			info->func = func;
			info->name = name;
			info->w_mem = mem;
			return (lib_thread)chThdCreateStatic(mem, stack_size, NORMALPRIO, lib_thd, info);
		}
	}

	return 0;
}

static void lib_request_terminate(lib_thread thd) {
	chThdTerminate((thread_t*)thd);

	int timeout = 2000;
	while (!chThdTerminatedX((thread_t*)thd)) {
		chThdSleepMilliseconds(1);
		timeout--;

		// Not terminating, reset using wdt to not start lbm after reboot.
		if (timeout == 0) {
			commands_printf_lisp("Not terminating, crashing...");
			chThdSleepMilliseconds(20);
			chSysLock();
			for (;;) {__NOP();}
		}
	}

}

static bool lib_should_terminate(void) {
	return chThdShouldTerminateX();
}

static void** lib_get_arg(uint32_t prog_addr) {
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].base_addr == prog_addr) {
			return &loaded_libs[i].arg;
		}
	}
	return 0;
}

static void lib_set_pad_mode(void *gpio, uint32_t pin, uint32_t mode) {
	palSetPadMode((stm32_gpio_t*)gpio, pin, mode);
}

static void lib_set_pad(void *gpio, uint32_t pin) {
	palSetPad((stm32_gpio_t*)gpio, pin);
}

static void lib_clear_pad(void *gpio, uint32_t pin) {
	palClearPad((stm32_gpio_t*)gpio, pin);
}

static bool get_gpio(VESC_PIN io, stm32_gpio_t **port, uint32_t *pin, bool *is_analog) {
	bool res = false;
	*is_analog = false;

	switch (io) {
	case VESC_PIN_COMM_RX:
#ifdef HW_UART_RX_PORT
		*port = HW_UART_RX_PORT; *pin = HW_UART_RX_PIN;
		res = true;
#endif
		break;
	case VESC_PIN_COMM_TX:
#ifdef HW_UART_TX_PORT
		*port = HW_UART_TX_PORT; *pin = HW_UART_TX_PIN;
		res = true;
#endif
		break;
	case VESC_PIN_SWDIO:
		*port = GPIOA; *pin = 13;
		res = true;
		break;
	case VESC_PIN_SWCLK:
		*port = GPIOA; *pin = 14;
		res = true;
		break;
	case VESC_PIN_HALL1:
		*port = HW_HALL_ENC_GPIO1; *pin = HW_HALL_ENC_PIN1;
		res = true;
		break;
	case VESC_PIN_HALL2:
		*port = HW_HALL_ENC_GPIO2; *pin = HW_HALL_ENC_PIN2;
		res = true;
		break;
	case VESC_PIN_HALL3:
		*port = HW_HALL_ENC_GPIO3; *pin = HW_HALL_ENC_PIN3;
		res = true;
		break;
	case VESC_PIN_ADC1:
#ifdef HW_ADC_EXT_GPIO
		*port = HW_ADC_EXT_GPIO; *pin = HW_ADC_EXT_PIN;
		*is_analog = true;
		res = true;
#endif
		break;
	case VESC_PIN_ADC2:
#ifdef HW_ADC_EXT2_GPIO
		*port = HW_ADC_EXT2_GPIO; *pin = HW_ADC_EXT2_PIN;
		*is_analog = true;
		res = true;
#endif
		break;
	case VESC_PIN_HALL4:
#ifdef HW_HALL_ENC_GPIO4
		*port = HW_HALL_ENC_GPIO4; *pin = HW_HALL_ENC_PIN4;
		res = true;
#endif
		break;
	case VESC_PIN_HALL5:
#ifdef HW_HALL_ENC_GPIO5
		*port = HW_HALL_ENC_GPIO5; *pin = HW_HALL_ENC_PIN5;
		res = true;
#endif
		break;
	case VESC_PIN_HALL6:
#ifdef HW_HALL_ENC_GPIO6
		*port = HW_HALL_ENC_GPIO6; *pin = HW_HALL_ENC_PIN6;
		res = true;
#endif
		break;
	}

	return res;
}

static bool lib_io_set_mode(VESC_PIN pin_vesc, VESC_PIN_MODE mode) {
	stm32_gpio_t *gpio;
	uint32_t pin;
	bool is_analog;
	bool res = false;

	if (get_gpio(pin_vesc, &gpio, &pin, &is_analog)) {
		switch (mode) {
		case VESC_PIN_MODE_INPUT_NOPULL:
			palSetPadMode(gpio, pin, PAL_MODE_INPUT);
			res = true;
			break;
		case VESC_PIN_MODE_INPUT_PULL_UP:
			palSetPadMode(gpio, pin, PAL_MODE_INPUT_PULLUP);
			res = true;
			break;
		case VESC_PIN_MODE_INPUT_PULL_DOWN:
			palSetPadMode(gpio, pin, PAL_MODE_INPUT_PULLDOWN);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_PUSHPULL);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT_OPEN_DRAIN:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_OPENDRAIN);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT_OPEN_DRAIN_PULL_UP:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLUP);
			res = true;
			break;
		case VESC_PIN_MODE_OUTPUT_OPEN_DRAIN_PULL_DOWN:
			palSetPadMode(gpio, pin, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLDOWN);
			res = true;
			break;
		case VESC_PIN_MODE_ANALOG:
			if (is_analog) {
				palSetPadMode(gpio, pin, PAL_STM32_MODE_ANALOG);
				res = true;
			}
			break;
		}
	}

	return res;
}

static bool lib_io_write(VESC_PIN pin_vesc, int state) {
	stm32_gpio_t *gpio;
	uint32_t pin;
	bool is_analog;
	bool res = false;

	if (get_gpio(pin_vesc, &gpio, &pin, &is_analog)) {
		palWritePad(gpio, pin, state);
		res = true;
	}

	return res;
}

static bool lib_io_read(VESC_PIN pin_vesc) {
	stm32_gpio_t *gpio;
	uint32_t pin;
	bool is_analog;
	bool res = false;

	if (get_gpio(pin_vesc, &gpio, &pin, &is_analog)) {
		res = palReadPad(gpio, pin);
	}

	return res;
}

static float lib_io_read_analog(VESC_PIN pin_vesc) {
	float res = -1.0;

	if (pin_vesc == VESC_PIN_ADC1) {
		res = ADC_VOLTS(ADC_IND_EXT);
	} else if (pin_vesc == VESC_PIN_ADC2) {
		res = ADC_VOLTS(ADC_IND_EXT2);
	}

	return res;
}

static bool lib_io_get_st_pin(VESC_PIN vesc_pin, void **gpio, uint32_t *pin) {
	bool analog;
	return get_gpio(vesc_pin, (stm32_gpio_t**)gpio, pin, &analog);
}

static SerialConfig uart_cfg = {
		2500000, 0, 0, 0
};

static bool lib_uart_start(uint32_t baudrate, bool half_duplex) {
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

	uart_cfg.speed = baudrate;
	uart_cfg.cr3 = half_duplex ? USART_CR3_HDSEL : 0;

	sdStop(&HW_UART_DEV);
	sdStart(&HW_UART_DEV, &uart_cfg);

	palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF));
	if (!half_duplex) {
		palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF));
	}

	return true;
}

static void wait_uart_tx_task(void *arg) {
	(void)arg;
	while(!chOQIsEmptyI(&HW_UART_DEV.oqueue)){
		chThdSleepMilliseconds(1);
	}
	chThdSleepMilliseconds(1);
	HW_UART_DEV.usart->CR1 |= USART_CR1_RE;
}

static bool lib_uart_write(uint8_t *data, uint32_t size) {
	if (uart_cfg.cr3 & USART_CR3_HDSEL) {
		HW_UART_DEV.usart->CR1 &= ~USART_CR1_RE;
		sdWrite(&HW_UART_DEV, data, size);
		worker_execute(wait_uart_tx_task, 0);
	} else{
		sdWrite(&HW_UART_DEV, data, size);
	}

	return true;
}

static int32_t lib_uart_read(void) {
	return sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
}

static float lib_ts_to_age_s(systime_t ts) {
	return UTILS_AGE_S(ts);
}

static float lib_get_cfg_float(CFG_PARAM p) {
	float res = 0.0;

	const volatile mc_configuration *conf = mc_interface_get_configuration();

	switch (p) {
		case CFG_PARAM_l_current_max: res = conf->l_current_max; break;
		case CFG_PARAM_l_current_min: res = conf->l_current_min; break;
		case CFG_PARAM_l_in_current_max: res = conf->l_in_current_max; break;
		case CFG_PARAM_l_in_current_min: res = conf->l_in_current_min; break;
		case CFG_PARAM_l_abs_current_max: res = conf->l_abs_current_max; break;
		case CFG_PARAM_l_min_erpm: res = conf->l_min_erpm; break;
		case CFG_PARAM_l_max_erpm: res = conf->l_max_erpm; break;
		case CFG_PARAM_l_erpm_start: res = conf->l_erpm_start; break;
		case CFG_PARAM_l_max_erpm_fbrake: res = conf->l_max_erpm_fbrake; break;
		case CFG_PARAM_l_max_erpm_fbrake_cc: res = conf->l_max_erpm_fbrake_cc; break;
		case CFG_PARAM_l_min_vin: res = conf->l_min_vin; break;
		case CFG_PARAM_l_max_vin: res = conf->l_max_vin; break;
		case CFG_PARAM_l_battery_cut_start: res = conf->l_battery_cut_start; break;
		case CFG_PARAM_l_battery_cut_end: res = conf->l_battery_cut_end; break;
		default: break;
	}

	return res;
}

static bool lib_create_byte_array(lbm_value *value, lbm_uint num_elt) {
	return lbm_heap_allocate_array(value, num_elt, LBM_TYPE_BYTE);
}

static bool lib_eval_is_paused(void) {
	return lbm_get_eval_state() == EVAL_CPS_STATE_PAUSED;
}

lbm_value ext_load_native_lib(lbm_value *args, lbm_uint argn) {
	lbm_value res = lbm_enc_sym(SYM_EERROR);

	if (argn != 1 || !lbm_is_array(args[0])) {
		return res;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	if (array->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	if (!lib_init_done) {
		memset((char*)cif.pad, 0, 2048);

		// LBM
		cif.cif.lbm_add_extension = lbm_add_extension;
		cif.cif.lbm_block_ctx_from_extension = lbm_block_ctx_from_extension;
		cif.cif.lbm_unblock_ctx = lbm_unblock_ctx;
		cif.cif.lbm_get_current_cid = lbm_get_current_cid;
		cif.cif.lbm_set_error_reason = lbm_set_error_reason;
		cif.cif.lbm_pause_eval_with_gc = lbm_pause_eval_with_gc;
		cif.cif.lbm_continue_eval = lbm_continue_eval;
		cif.cif.lbm_send_message = lbm_send_message;
		cif.cif.lbm_eval_is_paused = lib_eval_is_paused;

		cif.cif.lbm_cons = lbm_cons;
		cif.cif.lbm_car = lbm_car;
		cif.cif.lbm_cdr = lbm_cdr;
		cif.cif.lbm_list_destructive_reverse = lbm_list_destructive_reverse;
		cif.cif.lbm_create_byte_array = lib_create_byte_array;

		cif.cif.lbm_add_symbol_const = lbm_add_symbol_const;
		cif.cif.lbm_get_symbol_by_name = lbm_get_symbol_by_name;

		cif.cif.lbm_enc_i = lbm_enc_i;
		cif.cif.lbm_enc_u = lbm_enc_u;
		cif.cif.lbm_enc_char = lbm_enc_char;
		cif.cif.lbm_enc_float = lbm_enc_float;
		cif.cif.lbm_enc_u32 = lbm_enc_u32;
		cif.cif.lbm_enc_i32 = lbm_enc_i32;
		cif.cif.lbm_enc_sym = lbm_enc_sym;

		cif.cif.lbm_dec_as_float = lbm_dec_as_float;
		cif.cif.lbm_dec_as_u32 = lbm_dec_as_u32;
		cif.cif.lbm_dec_as_i32 = lbm_dec_as_i32;
		cif.cif.lbm_dec_char = lbm_dec_char;
		cif.cif.lbm_dec_str = lbm_dec_str;
		cif.cif.lbm_dec_sym = lbm_dec_sym;

		cif.cif.lbm_is_byte_array = lbm_is_byte_array;
		cif.cif.lbm_is_cons = lbm_is_cons;
		cif.cif.lbm_is_number = lbm_is_number;
		cif.cif.lbm_is_char = lbm_is_char;
		cif.cif.lbm_is_symbol = lbm_is_symbol;

		cif.cif.lbm_enc_sym_nil = ENC_SYM_NIL;
		cif.cif.lbm_enc_sym_true = ENC_SYM_TRUE;
		cif.cif.lbm_enc_sym_terror = ENC_SYM_TERROR;
		cif.cif.lbm_enc_sym_eerror = ENC_SYM_EERROR;
		cif.cif.lbm_enc_sym_merror = ENC_SYM_MERROR;

		cif.cif.lbm_is_symbol_nil = lbm_is_symbol_nil;
		cif.cif.lbm_is_symbol_true = lbm_is_symbol_true;

		// Os
		cif.cif.sleep_ms = lib_sleep_ms;
		cif.cif.sleep_us = lib_sleep_us;
		cif.cif.system_time = lib_system_time;
		cif.cif.ts_to_age_s = lib_ts_to_age_s;
		cif.cif.printf = commands_printf_lisp;
		cif.cif.malloc = lib_malloc;
		cif.cif.free = lib_free;
		cif.cif.spawn = lib_spawn;
		cif.cif.request_terminate = lib_request_terminate;
		cif.cif.should_terminate = lib_should_terminate;
		cif.cif.get_arg = lib_get_arg;

		// ST IO
		cif.cif.set_pad_mode = lib_set_pad_mode;
		cif.cif.set_pad = lib_set_pad;
		cif.cif.clear_pad = lib_clear_pad;

		// Abstract IO
		cif.cif.io_set_mode = lib_io_set_mode;
		cif.cif.io_write = lib_io_write;
		cif.cif.io_read = lib_io_read;
		cif.cif.io_read_analog = lib_io_read_analog;
		cif.cif.io_get_st_pin = lib_io_get_st_pin;

		// CAN
		cif.cif.can_set_sid_cb = comm_can_set_sid_rx_callback;
		cif.cif.can_set_eid_cb = comm_can_set_eid_rx_callback;
		cif.cif.can_transmit_sid = comm_can_transmit_sid;
		cif.cif.can_transmit_eid = comm_can_transmit_eid;
		cif.cif.can_send_buffer = comm_can_send_buffer;
		cif.cif.can_set_duty = comm_can_set_duty;
		cif.cif.can_set_current = comm_can_set_current;
		cif.cif.can_set_current_off_delay = comm_can_set_current_off_delay;
		cif.cif.can_set_current_brake = comm_can_set_current_brake;
		cif.cif.can_set_rpm = comm_can_set_rpm;
		cif.cif.can_set_pos = comm_can_set_pos;
		cif.cif.can_set_current_rel = comm_can_set_current_rel;
		cif.cif.can_set_current_rel_off_delay = comm_can_set_current_rel_off_delay;
		cif.cif.can_set_current_brake_rel = comm_can_set_current_brake_rel;
		cif.cif.can_ping = comm_can_ping;
		cif.cif.can_get_status_msg_index = comm_can_get_status_msg_index;
		cif.cif.can_get_status_msg_id = comm_can_get_status_msg_id;
		cif.cif.can_get_status_msg_2_index = comm_can_get_status_msg_2_index;
		cif.cif.can_get_status_msg_2_id = comm_can_get_status_msg_2_id;
		cif.cif.can_get_status_msg_3_index = comm_can_get_status_msg_3_index;
		cif.cif.can_get_status_msg_3_id = comm_can_get_status_msg_3_id;
		cif.cif.can_get_status_msg_4_index = comm_can_get_status_msg_4_index;
		cif.cif.can_get_status_msg_4_id = comm_can_get_status_msg_4_id;
		cif.cif.can_get_status_msg_5_index = comm_can_get_status_msg_5_index;
		cif.cif.can_get_status_msg_5_id = comm_can_get_status_msg_5_id;
		cif.cif.can_get_status_msg_6_index = comm_can_get_status_msg_6_index;
		cif.cif.can_get_status_msg_6_id = comm_can_get_status_msg_6_id;

		// Motor Control
		cif.cif.mc_motor_now = mc_interface_motor_now;
		cif.cif.mc_select_motor_thread = mc_interface_select_motor_thread;
		cif.cif.mc_get_motor_thread = mc_interface_get_motor_thread;
		cif.cif.mc_dccal_done = mc_interface_dccal_done;
		cif.cif.mc_set_pwm_callback = mc_interface_set_pwm_callback;
		cif.cif.mc_get_fault = mc_interface_get_fault;
		cif.cif.mc_fault_to_string = mc_interface_fault_to_string;
		cif.cif.mc_set_duty = mc_interface_set_duty;
		cif.cif.mc_set_duty_noramp = mc_interface_set_duty_noramp;
		cif.cif.mc_set_pid_speed = mc_interface_set_pid_speed;
		cif.cif.mc_set_pid_pos = mc_interface_set_pid_pos;
		cif.cif.mc_set_current = mc_interface_set_current;
		cif.cif.mc_set_brake_current = mc_interface_set_brake_current;
		cif.cif.mc_set_current_rel = mc_interface_set_current_rel;
		cif.cif.mc_set_brake_current_rel = mc_interface_set_brake_current_rel;
		cif.cif.mc_set_handbrake = mc_interface_set_handbrake;
		cif.cif.mc_set_handbrake_rel = mc_interface_set_handbrake_rel;
		cif.cif.mc_set_tachometer_value = mc_interface_set_tachometer_value;
		cif.cif.mc_release_motor = mc_interface_release_motor;
		cif.cif.mc_wait_for_motor_release = mc_interface_wait_for_motor_release;
		cif.cif.mc_get_duty_cycle_now = mc_interface_get_duty_cycle_now;
		cif.cif.mc_get_sampling_frequency_now = mc_interface_get_sampling_frequency_now;
		cif.cif.mc_get_rpm = mc_interface_get_rpm;
		cif.cif.mc_get_amp_hours = mc_interface_get_amp_hours;
		cif.cif.mc_get_amp_hours_charged = mc_interface_get_amp_hours_charged;
		cif.cif.mc_get_watt_hours = mc_interface_get_watt_hours;
		cif.cif.mc_get_watt_hours_charged = mc_interface_get_watt_hours_charged;
		cif.cif.mc_get_tot_current = mc_interface_get_tot_current;
		cif.cif.mc_get_tot_current_filtered = mc_interface_get_tot_current_filtered;
		cif.cif.mc_get_tot_current_directional = mc_interface_get_tot_current_directional;
		cif.cif.mc_get_tot_current_directional_filtered = mc_interface_get_tot_current_directional_filtered;
		cif.cif.mc_get_tot_current_in = mc_interface_get_tot_current_in;
		cif.cif.mc_get_tot_current_in_filtered = mc_interface_get_tot_current_in_filtered;
		cif.cif.mc_get_input_voltage_filtered = mc_interface_get_input_voltage_filtered;
		cif.cif.mc_get_tachometer_value = mc_interface_get_tachometer_value;
		cif.cif.mc_get_tachometer_abs_value = mc_interface_get_tachometer_abs_value;
		cif.cif.mc_get_pid_pos_set = mc_interface_get_pid_pos_set;
		cif.cif.mc_get_pid_pos_now = mc_interface_get_pid_pos_now;
		cif.cif.mc_update_pid_pos_offset = mc_interface_update_pid_pos_offset;
		cif.cif.mc_temp_fet_filtered = mc_interface_temp_fet_filtered;
		cif.cif.mc_temp_motor_filtered = mc_interface_temp_motor_filtered;
		cif.cif.mc_get_battery_level = mc_interface_get_battery_level;
		cif.cif.mc_get_speed = mc_interface_get_speed;
		cif.cif.mc_get_distance = mc_interface_get_distance;
		cif.cif.mc_get_distance_abs = mc_interface_get_distance_abs;
		cif.cif.mc_get_odometer = mc_interface_get_odometer;
		cif.cif.mc_set_odometer = mc_interface_set_odometer;
		cif.cif.mc_set_current_off_delay = mc_interface_set_current_off_delay;
		cif.cif.mc_stat_speed_avg = mc_interface_stat_speed_avg;
		cif.cif.mc_stat_speed_max = mc_interface_stat_speed_max;
		cif.cif.mc_stat_power_avg = mc_interface_stat_power_avg;
		cif.cif.mc_stat_power_max = mc_interface_stat_power_max;
		cif.cif.mc_stat_current_avg = mc_interface_stat_current_avg;
		cif.cif.mc_stat_current_max = mc_interface_stat_current_max;
		cif.cif.mc_stat_temp_mosfet_avg = mc_interface_stat_temp_mosfet_avg;
		cif.cif.mc_stat_temp_mosfet_max = mc_interface_stat_temp_mosfet_max;
		cif.cif.mc_stat_temp_motor_avg = mc_interface_stat_temp_motor_avg;
		cif.cif.mc_stat_temp_motor_max = mc_interface_stat_temp_motor_max;
		cif.cif.mc_stat_count_time = mc_interface_stat_count_time;
		cif.cif.mc_stat_reset = mc_interface_stat_reset;

		// Comm
		cif.cif.commands_process_packet = commands_process_packet;
		cif.cif.send_app_data = commands_send_app_data;
		cif.cif.set_app_data_handler = commands_set_app_data_handler;

		// UART
		cif.cif.uart_start = lib_uart_start;
		cif.cif.uart_write = lib_uart_write;
		cif.cif.uart_read = lib_uart_read;

		// Packets
		cif.cif.packet_init = packet_init;
		cif.cif.packet_reset = packet_reset;
		cif.cif.packet_process_byte = packet_process_byte;
		cif.cif.packet_send_packet = packet_send_packet;

		// IMU
		cif.cif.imu_startup_done = imu_startup_done;
		cif.cif.imu_get_roll = imu_get_roll;
		cif.cif.imu_get_pitch = imu_get_pitch;
		cif.cif.imu_get_yaw = imu_get_yaw;
		cif.cif.imu_get_rpy = imu_get_rpy;
		cif.cif.imu_get_accel = imu_get_accel;
		cif.cif.imu_get_gyro = imu_get_gyro;
		cif.cif.imu_get_mag = imu_get_mag;
		cif.cif.imu_derotate = imu_derotate;
		cif.cif.imu_get_accel_derotated = imu_get_accel_derotated;
		cif.cif.imu_get_gyro_derotated = imu_get_gyro_derotated;
		cif.cif.imu_get_quaternions = imu_get_quaternions;
		cif.cif.imu_get_calibration = imu_get_calibration;
		cif.cif.imu_set_yaw = imu_set_yaw;

		// Terminal
		cif.cif.terminal_register_command_callback = terminal_register_command_callback;
		cif.cif.terminal_unregister_callback = terminal_unregister_callback;

		// EEPROM
		cif.cif.read_eeprom_var = conf_general_read_eeprom_var_custom;
		cif.cif.store_eeprom_var = conf_general_store_eeprom_var_custom;

		// Timeout
		cif.cif.timeout_reset = timeout_reset;
		cif.cif.timeout_has_timeout = timeout_has_timeout;
		cif.cif.timeout_secs_since_update = timeout_secs_since_update;

		// Plot
		cif.cif.plot_init = commands_init_plot;
		cif.cif.plot_add_graph = commands_plot_add_graph;
		cif.cif.plot_set_graph = commands_plot_set_graph;
		cif.cif.plot_send_points = commands_send_plot_points;

		// Custom config
		cif.cif.conf_custom_add_config = conf_custom_add_config;
		cif.cif.conf_custom_clear_configs = conf_custom_clear_configs;

		// Settings
		cif.cif.get_cfg_float = lib_get_cfg_float;

		lib_init_done = true;
	}

	uint32_t addr = (uint32_t)array->data;

	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun != NULL && loaded_libs[i].base_addr == addr) {
			lbm_set_error_reason("Library already loaded");
			return res;
		}
	}

	bool ok = false;
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun == NULL) {
			loaded_libs[i].base_addr = addr;
			addr += 4; // Skip program pointer
			addr |= 1; // Ensure that thumb mode is used (??)
			ok = ((bool(*)(lib_info *info))addr)(&loaded_libs[i]);

			if (loaded_libs[i].stop_fun != NULL && !utils_is_func_valid(loaded_libs[i].stop_fun)) {
				loaded_libs[i].stop_fun = NULL;
				lbm_set_error_reason("Invalid stop function. Make sure that it is static.");
				return res;
			}

			break;
		}
	}

	if (ok) {
		res = lbm_enc_sym(SYM_TRUE);
	} else {
		lbm_set_error_reason("Library init failed");
	}

	return res;
}

lbm_value ext_unload_native_lib(lbm_value *args, lbm_uint argn) {
	lbm_value res = lbm_enc_sym(SYM_EERROR);

	if (argn != 1 || !lbm_is_array(args[0])) {
		return res;
	}

	lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
	if (array->elt_type != LBM_TYPE_BYTE) {
		return res;
	}

	uint32_t addr = (uint32_t)array->data;

	bool ok = false;
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun != NULL && loaded_libs[i].base_addr == addr) {
			loaded_libs[i].stop_fun(loaded_libs[i].arg);
			loaded_libs[i].stop_fun = NULL;
			res = lbm_enc_sym(SYM_TRUE);
			ok = true;
		}
	}

	if (!ok) {
		lbm_set_error_reason("Library not loaded");
	}

	return res;
}

void lispif_stop_lib(void) {
	for (int i = 0;i < LIB_NUM_MAX;i++) {
		if (loaded_libs[i].stop_fun != NULL) {
			loaded_libs[i].stop_fun(loaded_libs[i].arg);
			loaded_libs[i].stop_fun = NULL;
		}
	}
}
