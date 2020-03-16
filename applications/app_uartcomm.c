/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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

#include "app.h"
#include "ch.h"
#include "hal.h"
#include "hw.h"
#include "packet.h"
#include "commands.h"

#include <string.h>

// Settings
#define BAUDRATE					115200
#define PACKET_HANDLER				1
#define PACKET_HANDLER_P			2

// Threads
static THD_FUNCTION(packet_process_thread, arg);
static THD_WORKING_AREA(packet_process_thread_wa, 4096);

// Variables
static volatile bool thread_is_running = false;
static volatile bool uart_is_running = false;
static mutex_t send_mutex;
static bool send_mutex_init_done = false;

#ifdef HW_UART_P_DEV
static mutex_t send_mutex_p;
static bool send_mutex_p_init_done = false;
#endif

// Private functions
static void process_packet(unsigned char *data, unsigned int len);
static void send_packet(unsigned char *data, unsigned int len);

#ifdef HW_UART_P_DEV
static void process_packet_p(unsigned char *data, unsigned int len);
static void send_packet_p(unsigned char *data, unsigned int len);
#endif

static SerialConfig uart_cfg = {
		BAUDRATE,
		0,
		USART_CR2_LINEN,
		0
};

#ifdef HW_UART_P_DEV
static volatile bool from_p_uart = false;
static volatile bool uart_p_is_running = false;
static SerialConfig uart_p_cfg = {
		HW_UART_P_BAUD,
		0,
		USART_CR2_LINEN,
		0
};
#endif

static void process_packet(unsigned char *data, unsigned int len) {
	commands_process_packet(data, len, app_uartcomm_send_packet);
}

#ifdef HW_UART_P_DEV
static void process_packet_p(unsigned char *data, unsigned int len) {
	commands_process_packet(data, len, app_uartcomm_send_packet_p);
}
#endif

static void send_packet(unsigned char *data, unsigned int len) {
	if (uart_is_running) {
		sdWrite(&HW_UART_DEV, data, len);
	}
}

#ifdef HW_UART_P_DEV
static void send_packet_p(unsigned char *data, unsigned int len) {
	if (uart_p_is_running) {
#ifdef HW_UART_P_DEV_TX
		sdWrite(&HW_UART_P_DEV_TX, data, len);
#else
		sdWrite(&HW_UART_P_DEV, data, len);
#endif
	}
}
#endif

void app_uartcomm_start(void) {
	packet_init(send_packet, process_packet, PACKET_HANDLER);

	if (!thread_is_running) {
		chThdCreateStatic(packet_process_thread_wa, sizeof(packet_process_thread_wa),
				NORMALPRIO, packet_process_thread, NULL);
		thread_is_running = true;
	}

	sdStart(&HW_UART_DEV, &uart_cfg);
	palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
	palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);

	uart_is_running = true;
}

void app_uartcomm_start_permanent(void) {
#ifdef HW_UART_P_DEV
	packet_init(send_packet_p, process_packet_p, PACKET_HANDLER_P);

	if (!thread_is_running) {
		chThdCreateStatic(packet_process_thread_wa, sizeof(packet_process_thread_wa),
				NORMALPRIO, packet_process_thread, NULL);
		thread_is_running = true;
	}

	sdStart(&HW_UART_P_DEV, &uart_p_cfg);

#ifdef HW_UART_P_DEV_TX
	sdStart(&HW_UART_P_DEV_TX, &uart_p_cfg);
#endif

	palSetPadMode(HW_UART_P_TX_PORT, HW_UART_P_TX_PIN, PAL_MODE_ALTERNATE(HW_UART_P_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
	palSetPadMode(HW_UART_P_RX_PORT, HW_UART_P_RX_PIN, PAL_MODE_ALTERNATE(HW_UART_P_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);

	uart_p_is_running = true;
#endif
}

void app_uartcomm_stop(void) {
	if (uart_is_running) {
		sdStop(&HW_UART_DEV);
		palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_INPUT_PULLUP);
		uart_is_running = false;
	}

	// Notice that the processing thread is kept running in case this call is made from it.
}

void app_uartcomm_send_packet(unsigned char *data, unsigned int len) {
	if (!send_mutex_init_done) {
		chMtxObjectInit(&send_mutex);
		send_mutex_init_done = true;
	}

	chMtxLock(&send_mutex);
	packet_send_packet(data, len, PACKET_HANDLER);
	chMtxUnlock(&send_mutex);
}

void app_uartcomm_send_packet_p(unsigned char *data, unsigned int len) {
#ifdef HW_UART_P_DEV
	if (!send_mutex_p_init_done) {
		chMtxObjectInit(&send_mutex_p);
		send_mutex_p_init_done = true;
	}

	chMtxLock(&send_mutex_p);
	packet_send_packet(data, len, PACKET_HANDLER_P);
	chMtxUnlock(&send_mutex_p);
#else
	(void)data;
	(void)len;
#endif
}

void app_uartcomm_configure(uint32_t baudrate, bool permanent_enabled) {
	uart_cfg.speed = baudrate;

	if (thread_is_running && uart_is_running) {
		sdStart(&HW_UART_DEV, &uart_cfg);
	}

#ifdef HW_UART_P_DEV
	if (permanent_enabled) {
		palSetPadMode(HW_UART_P_TX_PORT, HW_UART_P_TX_PIN, PAL_MODE_ALTERNATE(HW_UART_P_GPIO_AF) |
				PAL_STM32_OSPEED_HIGHEST |
				PAL_STM32_PUDR_PULLUP);
		palSetPadMode(HW_UART_P_RX_PORT, HW_UART_P_RX_PIN, PAL_MODE_ALTERNATE(HW_UART_P_GPIO_AF) |
				PAL_STM32_OSPEED_HIGHEST |
				PAL_STM32_PUDR_PULLUP);
	} else {
		palSetPadMode(HW_UART_P_TX_PORT, HW_UART_P_TX_PIN, PAL_MODE_INPUT);
		palSetPadMode(HW_UART_P_RX_PORT, HW_UART_P_RX_PIN, PAL_MODE_INPUT);
	}
#else
	(void)permanent_enabled;
#endif
}

static THD_FUNCTION(packet_process_thread, arg) {
	(void)arg;

	chRegSetThreadName("uartcomm proc");

	event_listener_t el;
	chEvtRegisterMaskWithFlags(&HW_UART_DEV.event, &el, EVENT_MASK(0), CHN_INPUT_AVAILABLE);

#ifdef HW_UART_P_DEV
	event_listener_t elp;
	chEvtRegisterMaskWithFlags(&HW_UART_P_DEV.event, &elp, EVENT_MASK(0), CHN_INPUT_AVAILABLE);
#endif

	for(;;) {
		chEvtWaitAnyTimeout(ALL_EVENTS, ST2MS(10));

		bool rx = true;
		while (rx) {
			rx = false;

			if (uart_is_running) {
				msg_t res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
				if (res != MSG_TIMEOUT) {
#ifdef HW_UART_P_DEV
					from_p_uart = false;
#endif
					packet_process_byte(res, PACKET_HANDLER);
					rx = true;
				}
			}

#ifdef HW_UART_P_DEV
			msg_t res = sdGetTimeout(&HW_UART_P_DEV, TIME_IMMEDIATE);
			if (res != MSG_TIMEOUT) {
				from_p_uart = true;
				packet_process_byte(res, PACKET_HANDLER_P);
				rx = true;
			}
#endif
		}
	}
}
