/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#ifndef APP_H_
#define APP_H_

#include "conf_general.h"

// Functions
const app_configuration* app_get_configuration(void);
void app_set_configuration(app_configuration *conf);
void app_disable_output(int time_ms);
bool app_is_output_disabled(void);
unsigned short app_calc_crc(app_configuration* conf);

// Standard apps
void app_ppm_start(void);
void app_ppm_stop(void);
float app_ppm_get_decoded_level(void);
void app_ppm_detach(bool detach);
void app_ppm_override(float val);
void app_ppm_configure(ppm_config *conf);

void app_adc_start(bool use_rx_tx);
void app_adc_stop(void);
void app_adc_configure(adc_config *conf);
float app_adc_get_decoded_level(void);
float app_adc_get_voltage(void);
float app_adc_get_decoded_level2(void);
float app_adc_get_voltage2(void);
void app_adc_detach_adc(int detach);
void app_adc_adc1_override(float val);
void app_adc_adc2_override(float val);
void app_adc_detach_buttons(bool state);
void app_adc_rev_override(bool state);
void app_adc_cc_override(bool state);
bool app_adc_range_ok(void);

typedef enum {
	UART_PORT_COMM_HEADER = 0,
	UART_PORT_BUILTIN,
	UART_PORT_EXTRA_HEADER
} UART_PORT;

void app_uartcomm_initialize(void);
void app_uartcomm_start(UART_PORT port_number);
void app_uartcomm_stop(UART_PORT port_number);
void app_uartcomm_configure(uint32_t baudrate, bool permanent_enabled, UART_PORT port_number);
void app_uartcomm_send_packet(unsigned char *data, unsigned int len,  UART_PORT port_number);

void app_nunchuk_start(void);
void app_nunchuk_stop(void);
void app_nunchuk_configure(chuk_config *conf);
float app_nunchuk_get_decoded_x(void);
float app_nunchuk_get_decoded_y(void);
bool app_nunchuk_get_bt_c(void);
bool app_nunchuk_get_bt_z(void);
bool app_nunchuk_get_is_rev(void);
float app_nunchuk_get_update_age(void);
void app_nunchuk_update_output(chuck_data *data);

void app_pas_start(bool is_primary_output);
void app_pas_stop(void);
bool app_pas_is_running(void);
void app_pas_configure(pas_config *conf);
float app_pas_get_current_target_rel(void);
float app_pas_get_pedal_rpm(void);
void app_pas_set_current_sub_scaling(float current_sub_scaling);

// Custom apps
void app_custom_start(void);
void app_custom_stop(void);
void app_custom_configure(app_configuration *conf);

#endif /* APP_H_ */
