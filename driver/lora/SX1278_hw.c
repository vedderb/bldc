/**
 * Author Wojciech Domski <Wojciech.Domski@gmail.com>
 * www: www.Domski.pl
 *
 * Hardware layer for SX1278 LoRa module
 * modfied vor GESC schardt@team-ctech.de
 */

#include "conf_general.h"
#ifdef HW_HAS_LORA

#include "SX1278_hw.h"

static void spi_transfer(uint8_t *in_buf, const uint8_t *out_buf, int length);
static void spi_delay(void);


void SX1278_hw_init() {
	SX1278_hw_SetNSS(0);
	palSetPad(HW_LORA_SPI_PORT_RESET, HW_LORA_SPI_PIN_RESET);
}

void SX1278_hw_SetNSS(int value) {
	palWritePad(HW_LORA_SPI_PORT_NSS, HW_LORA_SPI_PIN_NSS, value);
}

void SX1278_hw_Reset() {
	SX1278_hw_SetNSS(1);
	palClearPad(HW_LORA_SPI_PORT_RESET, HW_LORA_SPI_PIN_RESET);
	SX1278_hw_DelayMs(1);
	palSetPad(HW_LORA_SPI_PORT_RESET, HW_LORA_SPI_PIN_RESET);
	SX1278_hw_DelayMs(100);
}

void SX1278_hw_SPICommand(uint8_t cmd) {
	uint8_t rx;
	SX1278_hw_SetNSS(0);
	spi_transfer(&rx, &cmd, 1); 
}

uint8_t SX1278_hw_SPIReadByte() {
	uint8_t rxByte = 0x00;

	SX1278_hw_SetNSS(0);
	spi_transfer(&rxByte, NULL, 1);
	return rxByte;
}

void SX1278_hw_DelayMs(uint32_t msec) {
	chThdSleepMilliseconds(msec);
}

int SX1278_hw_GetDIO0() {
	return (palReadPad(HW_LORA_SPI_PORT_DIO0,HW_LORA_SPI_PIN_DIO0));
}


// Software SPI
static void spi_transfer(uint8_t *in_buf, const uint8_t *out_buf, int length) {

	for (int i = 0;i < length;i++) {
		uint8_t send = out_buf ? out_buf[i] : 0xFF;
		uint8_t receive = 0;

		for (int bit = 0;bit < 8;bit++) {
			palWritePad(HW_LORA_SPI_PORT_MOSI, HW_LORA_SPI_PIN_MOSI, send >> 7);
			send <<= 1;

			palSetPad(HW_LORA_SPI_PORT_SCK, HW_LORA_SPI_PIN_SCK);
			spi_delay();

			int samples = 0;
			samples += palReadPad(HW_LORA_SPI_PORT_MISO, HW_LORA_SPI_PIN_MISO);
			__NOP();
			samples += palReadPad(HW_LORA_SPI_PORT_MISO, HW_LORA_SPI_PIN_MISO);
			__NOP();
			samples += palReadPad(HW_LORA_SPI_PORT_MISO, HW_LORA_SPI_PIN_MISO);
			__NOP();
			samples += palReadPad(HW_LORA_SPI_PORT_MISO, HW_LORA_SPI_PIN_MISO);
			__NOP();
			samples += palReadPad(HW_LORA_SPI_PORT_MISO, HW_LORA_SPI_PIN_MISO);
			receive <<= 1;
			if (samples > 2) {
				receive |= 1;
			}
			palClearPad(HW_LORA_SPI_PORT_SCK, HW_LORA_SPI_PIN_SCK);
			spi_delay();
		}
		if (in_buf) {
			in_buf[i] = receive;
		}
	}
}

static void spi_delay(void) {
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
}

#endif
