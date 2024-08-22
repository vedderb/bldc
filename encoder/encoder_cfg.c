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

#include "encoder_cfg.h"
#include "hw.h"
#include "ch.h"
#include "hal.h"

// Stack area for the running encoder
static THD_WORKING_AREA(encoder_thread_wa, 256);

#define SPI_BaudRatePrescaler_2         ((uint16_t)0x0000) //  42 MHz      21 MHZ
#define SPI_BaudRatePrescaler_4         ((uint16_t)0x0008) //  21 MHz      10.5 MHz
#define SPI_BaudRatePrescaler_8         ((uint16_t)0x0010) //  10.5 MHz    5.25 MHz
#define SPI_BaudRatePrescaler_16        ((uint16_t)0x0018) //  5.25 MHz    2.626 MHz
#define SPI_BaudRatePrescaler_32        ((uint16_t)0x0020) //  2.626 MHz   1.3125 MHz
#define SPI_BaudRatePrescaler_64        ((uint16_t)0x0028) //  1.3125 MHz  656.25 KHz
#define SPI_BaudRatePrescaler_128       ((uint16_t)0x0030) //  656.25 KHz  328.125 KHz
#define SPI_BaudRatePrescaler_256       ((uint16_t)0x0038) //  328.125 KHz 164.06 KHz
#define SPI_DATASIZE_8BIT				0
#define SPI_DATASIZE_16BIT				SPI_CR1_DFF

AS504x_config_t encoder_cfg_as504x = {
		{
				HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3,
				HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1,
#ifdef AS504x_MOSI_GPIO
				AS504x_MOSI_GPIO, AS504x_MOSI_PIN,
#else
				0, 0,
#endif
				HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2,
				{{NULL, NULL}, NULL, NULL} // Mutex
		},

		{0} // State
};

AD2S1205_config_t encoder_cfg_ad2s1205 = {
		{ // BB_SPI
				HW_SPI_PORT_NSS, HW_SPI_PIN_NSS,
				HW_SPI_PORT_SCK, HW_SPI_PIN_SCK,
#if defined(HW_SPI_PORT_MOSI) && AS504x_USE_SW_MOSI_PIN
				HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI,
#else
				0, 0,
#endif
				HW_SPI_PORT_MISO, HW_SPI_PIN_MISO,
				{{NULL, NULL}, NULL, NULL} // Mutex
		},
		{0},
};

MT6816_config_t encoder_cfg_mt6816 = {
#ifdef HW_SPI_DEV
		&HW_SPI_DEV, // spi_dev
		{//HARDWARE SPI CONFIG
				NULL, HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, SPI_BaudRatePrescaler_4 |
				SPI_CR1_CPOL | SPI_CR1_CPHA | SPI_DATASIZE_16BIT
		},

		HW_SPI_GPIO_AF,
		/*NSS*/HW_SPI_PORT_NSS, HW_SPI_PIN_NSS,
		/*SCK*/HW_SPI_PORT_SCK, HW_SPI_PIN_SCK,
		/*MOSI*/HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI,
		/*MISO*/HW_SPI_PORT_MISO, HW_SPI_PIN_MISO,
		{0, 0, 0, 0, 0, 0, 0},
#else
		0,
		{0},
		0,
		0, 0,
		0, 0,
		0, 0,
		0, 0,
		{0, 0, 0, 0, 0, 0, 0},
#endif
};

TLE5012_config_t encoder_cfg_tle5012 = {
		{
				HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, // nss
				HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, // sck
				HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, // mosi
				HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, // miso
				{{NULL, NULL}, NULL, NULL} // Mutex
		}, //ssc
		{0, 0, 0, 0, 0, 0, 0, 0} // State
};

ABI_config_t encoder_cfg_ABI = {
		10000, // counts
		HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1,
		HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2,
		HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3,
		HW_ENC_TIM,
		HW_ENC_TIM_AF,
		HW_ENC_EXTI_PORTSRC,
		HW_ENC_EXTI_PINSRC,
		HW_ENC_EXTI_LINE,
		HW_ENC_EXTI_CH,
		{0, 0}, // State
};

ENCSINCOS_config_t encoder_cfg_sincos = {0};

TS5700N8501_config_t encoder_cfg_TS5700N8501 = {
#if defined(HW_ADC_EXT_GPIO) && defined(HW_ADC_EXT_GPIO)
		&HW_UART_DEV,
		HW_UART_GPIO_AF,
		/*UART PINOUT*/
		HW_UART_TX_PORT, HW_UART_TX_PIN,
		HW_UART_RX_PORT, HW_UART_RX_PIN,
		HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN,
		{// UART CONFIG
				2500000,
				0,
				USART_CR2_LINEN,
				0
		},
#else
		0,
		0,
		0, 0,
		0, 0,
		0, 0,
		{0},
#endif
		encoder_thread_wa,
		sizeof(encoder_thread_wa),
		{0}
};

void enc_as5x47u_spi_callback(SPIDriver *pspi);
AS5x47U_config_t encoder_cfg_as5x47u = {
#ifdef HW_SPI_DEV
		&HW_SPI_DEV, // spi_dev
		{//HARDWARE SPI CONFIG
				enc_as5x47u_spi_callback, HW_SPI_PORT_NSS, HW_SPI_PIN_NSS, SPI_BaudRatePrescaler_8 |
				SPI_CR1_CPHA | SPI_DATASIZE_8BIT
		},

		HW_SPI_GPIO_AF,
		/*NSS*/HW_SPI_PORT_NSS, HW_SPI_PIN_NSS,
		/*SCK*/HW_SPI_PORT_SCK, HW_SPI_PIN_SCK,
		/*MOSI*/HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI,
		/*MISO*/HW_SPI_PORT_MISO, HW_SPI_PIN_MISO,
#else
		0,
		{0},
		0,
		0, 0,
		0, 0,
		0, 0,
		0, 0,
#endif
		{0}, // State
};

// Spi Handler for bissC
void compute_bissc_callback(SPIDriver *pspi);
BISSC_config_t encoder_cfg_bissc = {
#ifdef HW_SPI_DEV
		&HW_SPI_DEV, // spi_dev
		{//HARDWARE SPI CONFIG
				//NULL, HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, 
				&compute_bissc_callback, HW_SPI_PORT_NSS, HW_SPI_PIN_NSS, 
				SPI_BaudRatePrescaler_32 | SPI_CR1_CPOL | SPI_CR1_CPHA
		},

		HW_SPI_GPIO_AF,
		/*NSS*/HW_SPI_PORT_NSS, HW_SPI_PIN_NSS,
		/*SCK*/HW_SPI_PORT_SCK, HW_SPI_PIN_SCK,
		/*MOSI*/HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI,
		/*MISO*/HW_SPI_PORT_MISO, HW_SPI_PIN_MISO,
		22,   // enc_res
		{0}, // crc
		{0.0, 0, 0.0, 0, 0.0, 0, 0, {0}}
#else
		0,
		{0},
		0,
		0, 0,
		0, 0,
		0, 0,
		0, 0,
		22,   // enc_res
		{0}, // crc
		{0.0, 0, 0.0, 0, 0.0, 0, 0, {0}}
#endif
};
