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

#include "comm_can.h"
#include "ch.h"
#include "hal.h"
#include "datatypes.h"
#include "buffer.h"
#include "mcpwm.h"
#include "timeout.h"
#include "commands.h"
#include "app.h"

// Settings
#define CANDx			CAND1

// Threads
static WORKING_AREA(cancom_thread_wa, 1024);
static WORKING_AREA(cancom_status_thread_wa, 1024);
static msg_t cancom_thread(void *arg);
static msg_t cancom_status_thread(void *arg);

// Variables
static can_status_msg stat_msgs[CAN_STATUS_MSGS_TO_STORE];
static Mutex can_mtx;

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

		chThdSleepMilliseconds(CAN_STATUS_MSG_INT_MS);
	}

	return 0;
}

void comm_can_transmit(uint32_t id, uint8_t *data, uint8_t len) {
	CANTxFrame txmsg;

	txmsg.IDE = CAN_IDE_EXT;
	txmsg.EID = id;
	txmsg.RTR = CAN_RTR_DATA;
	txmsg.DLC = len;

	for (int i = 0;i < len;i++) {
		txmsg.data8[i] = data[i];
	}

	chMtxLock(&can_mtx);
	canTransmit(&CAND1, CAN_ANY_MAILBOX, &txmsg, MS2ST(2));
	chMtxUnlock();
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
