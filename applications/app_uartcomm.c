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

// Settings

// By default 7/8 pin uart is index 0, builtin BLE is index 1, and third uart is index 2
#define BAUDRATE					115200
#ifndef UART_NUMBER
#ifdef HW_UART_3_DEV
#define UART_NUMBER 3
#else
#ifdef HW_UART_P_DEV
#define UART_NUMBER 2
#else
#define UART_NUMBER 1
#endif
#endif
#endif

// Threads
static THD_FUNCTION(packet_process_thread, arg);
static THD_WORKING_AREA(packet_process_thread_wa, 2048);

// Variables
static volatile bool thread_is_running = false;
static volatile bool uart_is_running[UART_NUMBER] = {false};
static mutex_t send_mutex[UART_NUMBER];
static bool send_mutex_init_done[UART_NUMBER] = {false};
static SerialConfig uart_cfg[UART_NUMBER] = {{
		BAUDRATE,
		0,
		USART_CR2_LINEN,
		0
}};

// Different for Rx and Tx because it is possible for hardware to use different UART driver
// for Rx and Tx, feature not a bug XD
static SerialDriver *serialPortDriverTx[UART_NUMBER];
static SerialDriver *serialPortDriverRx[UART_NUMBER];
static stm32_gpio_t * TxGpioPort[UART_NUMBER];
static stm32_gpio_t * RxGpioPort[UART_NUMBER];
static uint8_t TxGpioPin[UART_NUMBER], RxGpioPin[UART_NUMBER], gpioAF[UART_NUMBER];
static PACKET_STATE_t packet_state[UART_NUMBER];
static bool pins_enabled[UART_NUMBER];

// Private functions
static void process_packet(unsigned char *data, unsigned int len, unsigned int port_number);
static void write_packet(unsigned char *data, unsigned int len, unsigned int port_number);

static void process_packet_1(unsigned char *data, unsigned int len) {process_packet(data,len,UART_PORT_COMM_HEADER);}
static void write_packet_1(unsigned char *data, unsigned int len) {write_packet(data,len,UART_PORT_COMM_HEADER);}
static void send_packet_1(unsigned char *data, unsigned int len) {app_uartcomm_send_packet(data,len,UART_PORT_COMM_HEADER);}

static void process_packet_2(unsigned char *data, unsigned int len) {process_packet(data,len,UART_PORT_BUILTIN);}
static void write_packet_2(unsigned char *data, unsigned int len) {write_packet(data,len,UART_PORT_BUILTIN);}
static void send_packet_2(unsigned char *data, unsigned int len) {app_uartcomm_send_packet(data,len,UART_PORT_BUILTIN);}

static void process_packet_3(unsigned char *data, unsigned int len) {process_packet(data,len,UART_PORT_EXTRA_HEADER);}
static void write_packet_3(unsigned char *data, unsigned int len) {write_packet(data,len,UART_PORT_EXTRA_HEADER);}
static void send_packet_3(unsigned char *data, unsigned int len) {app_uartcomm_send_packet(data,len,UART_PORT_EXTRA_HEADER);}

typedef void (*data_func) (unsigned char *data, unsigned int len);
static data_func write_functions[3] = {write_packet_1, write_packet_2, write_packet_3};
static data_func process_functions[3] = {process_packet_1, process_packet_2, process_packet_3};
static data_func send_functions[3] = {send_packet_1, send_packet_2, send_packet_3};

static void write_packet(unsigned char *data, unsigned int len, unsigned int port_number) {
	if (port_number >= UART_NUMBER) {
		return;
	}

	if (uart_is_running[port_number]) {
		sdWrite(serialPortDriverTx[port_number], data, len);
	}
}

static void process_packet(unsigned char *data, unsigned int len, unsigned int port_number) {
	if (port_number >= UART_NUMBER) {
		return;
	}

	commands_process_packet(data, len, send_functions[port_number]);
}

void app_uartcomm_initialize(void) {
	serialPortDriverTx[0] = &HW_UART_DEV;
	serialPortDriverRx[0] = &HW_UART_DEV;
	uart_cfg[0].speed =  BAUDRATE;
	RxGpioPort[0] = HW_UART_RX_PORT; RxGpioPin[0] = HW_UART_RX_PIN;
	TxGpioPort[0] = HW_UART_TX_PORT; TxGpioPin[0] = HW_UART_TX_PIN;
	gpioAF[0] = HW_UART_GPIO_AF;

#ifdef HW_UART_P_DEV
#ifdef HW_UART_P_DEV_TX
	serialPortDriverTx[1] = &HW_UART_P_DEV_TX;
#else
	serialPortDriverTx[1] =  &HW_UART_P_DEV;
#endif
	serialPortDriverRx[1] = &HW_UART_P_DEV;
	uart_cfg[1].speed = HW_UART_P_BAUD;
	RxGpioPort[1] = HW_UART_P_RX_PORT; RxGpioPin[1] = HW_UART_P_RX_PIN;
	TxGpioPort[1] = HW_UART_P_TX_PORT; TxGpioPin[1] = HW_UART_P_TX_PIN;
	gpioAF[1] = HW_UART_P_GPIO_AF;
#endif

#ifdef HW_UART_3_DEV
	serialPortDriverTx[2] =  &HW_UART_3_DEV;
	serialPortDriverRx[2] = &HW_UART_3_DEV;
	uart_cfg[2].speed = HW_UART_3_BAUD;
	RxGpioPort[2] = HW_UART_3_RX_PORT; RxGpioPin[2] = HW_UART_3_RX_PIN;
	TxGpioPort[2] = HW_UART_3_TX_PORT; TxGpioPin[2] = HW_UART_3_TX_PIN;
	gpioAF[2] = HW_UART_3_GPIO_AF;
#endif
}

void app_uartcomm_start(UART_PORT port_number) {
	if(port_number >= UART_NUMBER){
		return;
	}

	packet_init(write_functions[port_number], process_functions[port_number], &packet_state[port_number]);

	if (!thread_is_running) {
		chThdCreateStatic(packet_process_thread_wa, sizeof(packet_process_thread_wa),
				NORMALPRIO, packet_process_thread, NULL);
		thread_is_running = true;
	}

	sdStart(serialPortDriverRx[port_number], &uart_cfg[port_number]);
	sdStart(serialPortDriverTx[port_number], &uart_cfg[port_number]);
	uart_is_running[port_number] = true;

	palSetPadMode(TxGpioPort[port_number], TxGpioPin[port_number], PAL_MODE_ALTERNATE(gpioAF[port_number]) |
			PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
	palSetPadMode(RxGpioPort[port_number], RxGpioPin[port_number], PAL_MODE_ALTERNATE(gpioAF[port_number]) |
			PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
	pins_enabled[port_number] = true;
}

void app_uartcomm_stop(UART_PORT port_number) {
	if(port_number >= UART_NUMBER) {
		return;
	}

	if (uart_is_running[port_number]) {
		sdStop(serialPortDriverRx[port_number]);
		sdStop(serialPortDriverTx[port_number]);
		palSetPadMode(TxGpioPort[port_number], TxGpioPin[port_number], PAL_MODE_INPUT_PULLUP);
		palSetPadMode(RxGpioPort[port_number], RxGpioPin[port_number], PAL_MODE_INPUT_PULLUP);
		uart_is_running[port_number] = false;
	}
	// Notice that the processing thread is kept running in case this call is made from it.
}

void app_uartcomm_send_packet(unsigned char *data, unsigned int len, UART_PORT port_number) {
	if (port_number >= UART_NUMBER) {
		return;
	}

	if (!send_mutex_init_done[port_number]) {
		chMtxObjectInit(&send_mutex[port_number]);
		send_mutex_init_done[port_number] = true;
	}

	chMtxLock(&send_mutex[port_number]);
	packet_send_packet(data, len, &packet_state[port_number]);
	chMtxUnlock(&send_mutex[port_number]);
}

void app_uartcomm_configure(uint32_t baudrate, bool enabled, UART_PORT port_number) {
	if (port_number >= UART_NUMBER) {
		return;
	}

	if (baudrate > 0) {
		uart_cfg[port_number].speed = baudrate;
	}

	if (thread_is_running && uart_is_running[port_number]) {
		sdStart(serialPortDriverRx[port_number], &uart_cfg[port_number]);

		if (enabled && !pins_enabled[port_number]) {
			palSetPadMode(TxGpioPort[port_number], TxGpioPin[port_number], PAL_MODE_ALTERNATE(gpioAF[port_number]) |
					PAL_STM32_OSPEED_HIGHEST |
					PAL_STM32_PUDR_PULLUP);
			palSetPadMode(RxGpioPort[port_number], RxGpioPin[port_number], PAL_MODE_ALTERNATE(gpioAF[port_number]) |
					PAL_STM32_OSPEED_HIGHEST |
					PAL_STM32_PUDR_PULLUP);
			pins_enabled[port_number] = true;
		} else if (!enabled && pins_enabled[port_number]) {
			palSetPadMode(TxGpioPort[port_number], TxGpioPin[port_number], PAL_MODE_INPUT);
			palSetPadMode(RxGpioPort[port_number], RxGpioPin[port_number], PAL_MODE_INPUT);
			pins_enabled[port_number] = false;
		}
	}
}

static THD_FUNCTION(packet_process_thread, arg) {
	(void)arg;

	chRegSetThreadName("uartcomm proc");

	event_listener_t el[UART_NUMBER];
	for(int port_number = 0; port_number < UART_NUMBER; port_number++) {
		chEvtRegisterMaskWithFlags(&(*serialPortDriverRx[port_number]).event, &el[port_number], EVENT_MASK(0), CHN_INPUT_AVAILABLE);
	}

	for(;;) {
		chEvtWaitAnyTimeout(ALL_EVENTS, ST2MS(10));

		bool rx = true;
		while (rx) {
			rx = false;
			for(int port_number = 0; port_number < UART_NUMBER; port_number++) {
				if (uart_is_running[port_number]) {
					msg_t res = sdGetTimeout(serialPortDriverRx[port_number], TIME_IMMEDIATE);
					if (res != MSG_TIMEOUT) {
						packet_process_byte(res, &packet_state[port_number]);
						rx = true;
					}
				}
			}
		}
	}
}

