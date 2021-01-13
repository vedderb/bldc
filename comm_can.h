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

#ifndef COMM_CAN_H_
#define COMM_CAN_H_

#include "conf_general.h"
#include "hal.h"

// Settings
#define CAN_STATUS_MSGS_TO_STORE	10

// Functions
void comm_can_init(void);
void comm_can_set_baud(CAN_BAUD baud);
void comm_can_transmit_eid(uint32_t id, const uint8_t *data, uint8_t len);
void comm_can_transmit_eid_replace(uint32_t id, const uint8_t *data, uint8_t len, bool replace);
void comm_can_transmit_sid(uint32_t id, uint8_t *data, uint8_t len);
void comm_can_set_sid_rx_callback(bool (*p_func)(uint32_t id, uint8_t *data, uint8_t len));
void comm_can_set_eid_rx_callback(bool (*p_func)(uint32_t id, uint8_t *data, uint8_t len));
void comm_can_send_buffer(uint8_t controller_id, uint8_t *data, unsigned int len, uint8_t send);
void comm_can_set_duty(uint8_t controller_id, float duty);
void comm_can_set_current(uint8_t controller_id, float current);
void comm_can_set_current_brake(uint8_t controller_id, float current);
void comm_can_set_rpm(uint8_t controller_id, float rpm);
void comm_can_set_pos(uint8_t controller_id, float pos);
void comm_can_set_current_rel(uint8_t controller_id, float current_rel);
void comm_can_set_current_brake_rel(uint8_t controller_id, float current_rel);
bool comm_can_ping(uint8_t controller_id, HW_TYPE *hw_type);
void comm_can_detect_apply_all_foc(uint8_t controller_id, bool activate_status_msgs, float max_power_loss);
void comm_can_conf_current_limits(uint8_t controller_id,
		bool store, float min, float max);
void comm_can_conf_current_limits_in(uint8_t controller_id,
		bool store, float min, float max);
void comm_can_conf_foc_erpms(uint8_t controller_id,
		bool store, float foc_openloop_rpm, float foc_sl_erpm);
int comm_can_detect_all_foc_res(unsigned int index);
int comm_can_detect_all_foc_res_size(void);
void comm_can_detect_all_foc_res_clear(void);
void comm_can_conf_battery_cut(uint8_t controller_id,
		bool store, float start, float end);
void comm_can_shutdown(uint8_t controller_id);
can_status_msg *comm_can_get_status_msg_index(int index);
can_status_msg *comm_can_get_status_msg_id(int id);
can_status_msg_2 *comm_can_get_status_msg_2_index(int index);
can_status_msg_2 *comm_can_get_status_msg_2_id(int id);
can_status_msg_3 *comm_can_get_status_msg_3_index(int index);
can_status_msg_3 *comm_can_get_status_msg_3_id(int id);
can_status_msg_4 *comm_can_get_status_msg_4_index(int index);
can_status_msg_4 *comm_can_get_status_msg_4_id(int id);
can_status_msg_5 *comm_can_get_status_msg_5_index(int index);
can_status_msg_5 *comm_can_get_status_msg_5_id(int id);

io_board_adc_values *comm_can_get_io_board_adc_1_4_index(int index);
io_board_adc_values *comm_can_get_io_board_adc_1_4_id(int id);
io_board_adc_values *comm_can_get_io_board_adc_5_8_index(int index);
io_board_adc_values *comm_can_get_io_board_adc_5_8_id(int id);
io_board_digial_inputs *comm_can_get_io_board_digital_in_index(int index);
io_board_digial_inputs *comm_can_get_io_board_digital_in_id(int id);
void comm_can_io_board_set_output_digital(int id, int channel, bool on);
void comm_can_io_board_set_output_pwm(int id, int channel, float duty);

CANRxFrame *comm_can_get_rx_frame(void);

#endif /* COMM_CAN_H_ */
