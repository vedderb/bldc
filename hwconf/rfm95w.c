/*

	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se
    modified 2021 by Georg Schardt schardt@team-ctech.de

    Use SPI Port to communicate with e.g. RFM95W Modul

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

#include "conf_general.h"
#ifdef HW_HAS_RFM95W

#include "app.h"
#include "ch.h"
#include "hal.h"
#include "hw.h"
#include "packet.h"
#include "commands.h"
#include "rfm95w.h"
#include <string.h>
#include "SX1278.h"
#include "stdio.h"

// Settings
#define HW_SPI_FREQUENCY    8E6
#define PACKET_HANDLER      3

// Threads
static THD_FUNCTION(packet_process_thread, arg);
static THD_WORKING_AREA(packet_process_thread_wa, 4096);

// Variables
static volatile bool thread_is_running = false;
static volatile bool spi_is_running = false;
static mutex_t send_mutex;
static bool send_mutex_init_done = false;
SX1278_t SX1278;

// Private functions
static void process_packet(unsigned char *data, unsigned int len);
static void send_packet(unsigned char *data, unsigned int len);


static void process_packet(unsigned char *data, unsigned int len) {
//	commands_process_packet(data, len, rfm95w_send_packet);
}

static void send_packet(unsigned char *data, unsigned int len) {
	if (spi_is_running) {
      SX1278_LoRaEntryTx(&SX1278, len, 200);
      SX1278_LoRaTxPacket(&SX1278, (uint8_t*) data, len, 200);
	}
}

void rfm95w_init(void) {
//    packet_init(send_packet, process_packet, PACKET_HANDLER);

    palSetPadMode(HW_RFM95W_SPI_PORT_SCK, HW_RFM95W_SPI_PIN_SCK, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
    palSetPadMode(HW_RFM95W_SPI_PORT_MISO, HW_RFM95W_SPI_PIN_MISO,PAL_MODE_INPUT);
    palSetPadMode(HW_RFM95W_SPI_PORT_NSS, HW_RFM95W_SPI_PIN_NSS, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
    palSetPadMode(HW_RFM95W_SPI_PORT_MOSI, HW_RFM95W_SPI_PIN_MOSI, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
    palSetPadMode(HW_RFM95W_SPI_PORT_DIO0, HW_RFM95W_SPI_PIN_DIO0, PAL_MODE_INPUT);
    palSetPadMode(HW_RFM95W_SPI_PORT_RESET, HW_RFM95W_SPI_PIN_RESET, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

    SX1278_init(&SX1278, 866000000, SX1278_POWER_17DBM, SX1278_LORA_SF_10, SX1278_LORA_BW_125KHZ, SX1278_LORA_CR_4_5, SX1278_LORA_CRC_DIS, 1);
	if (!thread_is_running) {
		chThdCreateStatic(packet_process_thread_wa, sizeof(packet_process_thread_wa), NORMALPRIO, packet_process_thread, NULL);
		thread_is_running = true;
	}
}

void rfm95_stop(void) {
	if (spi_is_running) {
        spiStop(&HW_RFM95W_SPI_DEV);
		spi_is_running = false;
	}
}

void rfm95w_send_packet(unsigned char *data, unsigned int len) {
	if (!send_mutex_init_done) {
		chMtxObjectInit(&send_mutex);
		send_mutex_init_done = true;
	}

	chMtxLock(&send_mutex);
	packet_send_packet(data, len, PACKET_HANDLER);
	chMtxUnlock(&send_mutex);
}

static THD_FUNCTION(packet_process_thread, arg) {
	(void)arg;

    unsigned char buffer[128];
	chRegSetThreadName("rfm95w proc");

    buffer[0]=COMM_GET_VALUES;
	for(;;) {
//    	commands_process_packet(buffer, 1, rfm95w_send_packet);
        chThdSleepMilliseconds(500);
	}
}
#endif


