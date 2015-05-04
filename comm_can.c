/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * comm_can.c
 *
 *  Created on: 7 dec 2014
 *      Author: benjamin
 */

#include <string.h>
#include "comm_can.h"
#include "ch.h"
#include "hal.h"
#include "datatypes.h"
#include "buffer.h"
#include "mcpwm.h"
#include "timeout.h"
#include "commands.h"
#include "app.h"
#include "crc.h"

// Settings
#define CANDx			CAND1

// Threads
static WORKING_AREA(cancom_thread_wa, 4096);
static WORKING_AREA(cancom_status_thread_wa, 1024);
static msg_t cancom_thread(void *arg);
static msg_t cancom_status_thread(void *arg);

// Variables
static can_status_msg stat_msgs[CAN_STATUS_MSGS_TO_STORE];
static Mutex can_mtx;
static uint8_t rx_buffer[256];
static uint8_t rx_buffer_last_id;

/*
 * 500KBaud, automatic wakeup, automatic recover
 * from abort mode.
 * See section 22.7.7 on the STM32 reference manual.
 */
static const CANConfig cancfg = {
		CAN_MCR_ABOM | CAN_MCR_AWUM | CAN_MCR_TXFP,
		CAN_BTR_SJW(0) | CAN_BTR_TS2(1) |
		CAN_BTR_TS1(8) | CAN_BTR_BRP(6)
};

// Private functions
static void send_packet_wrapper(unsigned char *data, unsigned char len);

void comm_can_init(void) {
	for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
		stat_msgs[i].id = -1;
	}

	chMtxInit(&can_mtx);

	palSetPadMode(GPIOB, 8,
			PAL_MODE_ALTERNATE(GPIO_AF_CAN1) |
			PAL_STM32_OTYPE_PUSHPULL |
			PAL_STM32_OSPEED_MID1);
	palSetPadMode(GPIOB, 9,
			PAL_MODE_ALTERNATE(GPIO_AF_CAN1) |
			PAL_STM32_OTYPE_PUSHPULL |
			PAL_STM32_OSPEED_MID1);

	chMtxLock(&can_mtx);
	canStart(&CANDx, &cancfg);
	chMtxUnlock();

	chThdCreateStatic(cancom_thread_wa, sizeof(cancom_thread_wa), NORMALPRIO,
			cancom_thread, NULL);
	chThdCreateStatic(cancom_status_thread_wa, sizeof(cancom_status_thread_wa), NORMALPRIO,
			cancom_status_thread, NULL);
}

static msg_t cancom_thread(void *arg) {
	(void)arg;
	chRegSetThreadName("CAN");

	EventListener el;
	CANRxFrame rxmsg;
	int32_t ind = 0;
	int32_t rxbuf_len;
	uint8_t crc_low;
	uint8_t crc_high;
	bool commands_send;

	chEvtRegister(&CANDx.rxfull_event, &el, 0);

	while(!chThdShouldTerminate()) {
		if (chEvtWaitAnyTimeout(ALL_EVENTS, MS2ST(10)) == 0) {
			continue;
		}

		chMtxLock(&can_mtx);
		msg_t result = canReceive(&CANDx, CAN_ANY_MAILBOX, &rxmsg, TIME_IMMEDIATE);
		chMtxUnlock();

		while (result == RDY_OK) {
			if (rxmsg.IDE == CAN_IDE_EXT) {
				uint8_t id = rxmsg.EID & 0xFF;
				CAN_PACKET_ID cmd = rxmsg.EID >> 8;
				can_status_msg *stat_tmp;

				if (id == 255 || id == app_get_configuration()->controller_id) {
					switch (cmd) {
					case CAN_PACKET_SET_DUTY:
						ind = 0;
						mcpwm_set_duty((float)buffer_get_int32(rxmsg.data8, &ind) / 100000.0);
						timeout_reset();
						break;

					case CAN_PACKET_SET_CURRENT:
						ind = 0;
						mcpwm_set_current((float)buffer_get_int32(rxmsg.data8, &ind) / 1000.0);
						timeout_reset();
						break;

					case CAN_PACKET_SET_CURRENT_BRAKE:
						ind = 0;
						mcpwm_set_brake_current((float)buffer_get_int32(rxmsg.data8, &ind) / 1000.0);
						timeout_reset();
						break;

					case CAN_PACKET_SET_RPM:
						ind = 0;
						mcpwm_set_pid_speed((float)buffer_get_int32(rxmsg.data8, &ind));
						timeout_reset();
						break;

					case CAN_PACKET_SET_POS:
						ind = 0;
						mcpwm_set_pid_pos((float)buffer_get_int32(rxmsg.data8, &ind) / 1000000.0);
						timeout_reset();
						break;

					case CAN_PACKET_FILL_RX_BUFFER:
						memcpy(rx_buffer + rxmsg.data8[0], rxmsg.data8 + 1, rxmsg.DLC - 1);
						break;

					case CAN_PACKET_PROCESS_RX_BUFFER:
						ind = 0;
						rx_buffer_last_id = rxmsg.data8[ind++];
						commands_send = rxmsg.data8[ind++];
						rxbuf_len = rxmsg.data8[ind++];
						crc_high = rxmsg.data8[ind++];
						crc_low = rxmsg.data8[ind++];

						if (crc16(rx_buffer, rxbuf_len)
								== ((unsigned short) crc_high << 8
										| (unsigned short) crc_low)) {

							if (commands_send) {
								commands_send_packet(rx_buffer, rxbuf_len);
							} else {
								commands_set_send_func(send_packet_wrapper);
								commands_process_packet(rx_buffer, rxbuf_len);
							}
						}
						break;

					case CAN_PACKET_PROCESS_SHORT_BUFFER:
						ind = 0;
						rx_buffer_last_id = rxmsg.data8[ind++];
						commands_send = rxmsg.data8[ind++];

						if (commands_send) {
							commands_send_packet(rxmsg.data8 + ind, rxmsg.DLC - ind);
						} else {
							commands_set_send_func(send_packet_wrapper);
							commands_process_packet(rxmsg.data8 + ind, rxmsg.DLC - ind);
						}
						break;

					default:
						break;
					}
				}

				switch (cmd) {
				case CAN_PACKET_STATUS:
					for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
						stat_tmp = &stat_msgs[i];
						if (stat_tmp->id == id || stat_tmp->id == -1) {
							ind = 0;
							stat_tmp->id = id;
							stat_tmp->rx_time = chTimeNow();
							stat_tmp->rpm = (float)buffer_get_int32(rxmsg.data8, &ind);
							stat_tmp->current = (float)buffer_get_int16(rxmsg.data8, &ind) / 10.0;
							stat_tmp->duty = (float)buffer_get_int16(rxmsg.data8, &ind) / 1000.0;
							break;
						}
					}
					break;

				default:
					break;
				}
			}

			chMtxLock(&can_mtx);
			result = canReceive(&CANDx, CAN_ANY_MAILBOX, &rxmsg, TIME_IMMEDIATE);
			chMtxUnlock();
		}
	}

	chEvtUnregister(&CAND1.rxfull_event, &el);
	return 0;
}

static msg_t cancom_status_thread(void *arg) {
	(void)arg;
	chRegSetThreadName("CAN status");

	for(;;) {
		if (app_get_configuration()->send_can_status) {
			// Send status message
			int32_t send_index = 0;
			uint8_t buffer[8];
			buffer_append_int32(buffer, (int32_t)mcpwm_get_rpm(), &send_index);
			buffer_append_int16(buffer, (int16_t)(mcpwm_get_tot_current() * 10.0), &send_index);
			buffer_append_int16(buffer, (int16_t)(mcpwm_get_duty_cycle_now() * 1000.0), &send_index);
			comm_can_transmit(app_get_configuration()->controller_id | ((uint32_t)CAN_PACKET_STATUS << 8), buffer, send_index);
		}

		systime_t sleep_time = CH_FREQUENCY / app_get_configuration()->send_can_status_rate_hz;
		if (sleep_time == 0) {
			sleep_time = 1;
		}

		chThdSleep(sleep_time);
	}

	return 0;
}

void comm_can_transmit(uint32_t id, uint8_t *data, uint8_t len) {
#if CAN_ENABLE
	chMtxLock(&can_mtx);

	static CANTxFrame txmsg[10];
	static int txmsg_ind = 0;

	txmsg[txmsg_ind].IDE = CAN_IDE_EXT;
	txmsg[txmsg_ind].EID = id;
	txmsg[txmsg_ind].RTR = CAN_RTR_DATA;
	txmsg[txmsg_ind].DLC = len;

	memcpy(txmsg[txmsg_ind].data8, data, len);

	canTransmit(&CAND1, CAN_ANY_MAILBOX, &txmsg[txmsg_ind], MS2ST(50));

	txmsg_ind++;
	if (txmsg_ind >= 10) {
		txmsg_ind = 0;
	}

	chMtxUnlock();
#else
	(void)id;
	(void)data;
	(void)len;
#endif
}

/**
 * Send a buffer up to 256 bytes as fragments. If the buffer is 6 bytes or less
 * it will be sent in a single CAN frame, otherwise it will be split into
 * several frames.
 *
 * @param controller_id
 * The controller id to send to.
 *
 * @param data
 * The payload.
 *
 * @param len
 * The payload length.
 *
 * @param send
 * If true, this packet will be passed to the send function of commands.
 * Otherwise, it will be passed to the process function.
 */
void comm_can_send_buffer(uint8_t controller_id, uint8_t *data, uint8_t len, bool send) {
	uint8_t send_buffer[8];

	if (len <= 6) {
		uint32_t ind = 0;
		send_buffer[ind++] = app_get_configuration()->controller_id;
		send_buffer[ind++] = send;
		memcpy(send_buffer + ind, data, len);
		ind += len;
		comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_PROCESS_SHORT_BUFFER << 8), send_buffer, ind);
	} else {
		for (int i = 0;i < len;i += 7) {
			uint8_t send_len = 7;

			send_buffer[0] = i;

			if ((i + 7) <= len) {
				memcpy(send_buffer + 1, data + i, send_len);
			} else {
				send_len = len - i;
				memcpy(send_buffer + 1, data + i, send_len);
			}

			comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_FILL_RX_BUFFER << 8), send_buffer, send_len + 1);
		}

		uint32_t ind = 0;
		send_buffer[ind++] = app_get_configuration()->controller_id;
		send_buffer[ind++] = send;
		send_buffer[ind++] = len;
		unsigned short crc = crc16(data, len);
		send_buffer[ind++] = (uint8_t)(crc >> 8);
		send_buffer[ind++] = (uint8_t)(crc & 0xFF);

		comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_PROCESS_RX_BUFFER << 8), send_buffer, 5);
	}
}

void comm_can_set_duty(uint8_t controller_id, float duty) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(duty * 100000.0), &send_index);
	comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_SET_DUTY << 8), buffer, send_index);
}

void comm_can_set_current(uint8_t controller_id, float current) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(current * 1000.0), &send_index);
	comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_SET_CURRENT << 8), buffer, send_index);
}

void comm_can_set_current_brake(uint8_t controller_id, float current) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(current * 1000.0), &send_index);
	comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_SET_CURRENT_BRAKE << 8), buffer, send_index);
}

void comm_can_set_rpm(uint8_t controller_id, float rpm) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)rpm, &send_index);
	comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_SET_RPM << 8), buffer, send_index);
}

void comm_can_set_pos(uint8_t controller_id, float pos) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(pos * 1000000.0), &send_index);
	comm_can_transmit(controller_id | ((uint32_t)CAN_PACKET_SET_POS << 8), buffer, send_index);
}

/**
 * Get status message by index.
 *
 * @param index
 * Index in the array
 *
 * @return
 * The message or 0 for an invalid index.
 */
can_status_msg *comm_can_get_status_msg_index(int index) {
	if (index < CAN_STATUS_MSGS_TO_STORE) {
		return &stat_msgs[index];
	} else {
		return 0;
	}
}

/**
 * Get status message by id.
 *
 * @param id
 * Id of the controller that sent the status message.
 *
 * @return
 * The message or 0 for an invalid id.
 */
can_status_msg *comm_can_get_status_msg_id(int id) {
	for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
		if (stat_msgs[i].id == id) {
			return &stat_msgs[i];
		}
	}

	return 0;
}

static void send_packet_wrapper(unsigned char *data, unsigned char len) {
	comm_can_send_buffer(rx_buffer_last_id, data, len, true);
}
