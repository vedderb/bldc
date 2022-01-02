#ifndef ENCODER_ENCODER_HWCONF_H_
#define ENCODER_ENCODER_HWCONF_H_

#include "encoder/encoder_datatype.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

#define ENCSPI_SAMPLE_RATE_HZ		20000

#define ENCODERS_CONFIG_UNUSED {0}
#define ENCODERS_VAR_UNINITIALIZED 0
#define ENCODERS_STRUCT_UNDEFINED {0}
#define ENCODERS_ABI_COUNTER_DEFAULT_VALUE 10000ul

#define SPI_BaudRatePrescaler_2         ((uint16_t)0x0000) //  42 MHz      21 MHZ
#define SPI_BaudRatePrescaler_4         ((uint16_t)0x0008) //  21 MHz      10.5 MHz
#define SPI_BaudRatePrescaler_8         ((uint16_t)0x0010) //  10.5 MHz    5.25 MHz
#define SPI_BaudRatePrescaler_16        ((uint16_t)0x0018) //  5.25 MHz    2.626 MHz
#define SPI_BaudRatePrescaler_32        ((uint16_t)0x0020) //  2.626 MHz   1.3125 MHz
#define SPI_BaudRatePrescaler_64        ((uint16_t)0x0028) //  1.3125 MHz  656.25 KHz
#define SPI_BaudRatePrescaler_128       ((uint16_t)0x0030) //  656.25 KHz  328.125 KHz
#define SPI_BaudRatePrescaler_256       ((uint16_t)0x0038) //  328.125 KHz 164.06 KHz
#define SPI_DATASIZE_16BIT				SPI_CR1_DFF

ENCSPI_config_t encoders_conf_ENCSPI =
{
  ENCODERS_VAR_UNINITIALIZED,
  ENCSPI_SAMPLE_RATE_HZ,
  {/*SPI PINOUT*/
    {/*NSS*/HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3 },
	{/*MISO*/HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2 },
	{/*MOSI*/HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI },
	{/*SCK*/HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1 }
  },
#ifdef HW_SPI_DEV
  {//HARDWARE SPI CONFIG
    NULL, HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, SPI_BaudRatePrescaler_4 | SPI_CR1_CPOL | SPI_CR1_CPHA
					| SPI_DATASIZE_16BIT
  }
#else
	ENCODERS_CONFIG_UNUSED
#endif
};

ABI_config_t encoders_conf_ABI =
{
  ENCODERS_VAR_UNINITIALIZED,
  ENCODERS_ABI_COUNTER_DEFAULT_VALUE,
  {/*INCREMENTAL PROTOCOL PINOUT*/
    {/*A*/HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1 },
    {/*B*/HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2 }
  }
};

ENCSINCOS_config_t encoders_conf_ENCSINCOS = ENCODERS_STRUCT_UNDEFINED;

TS5700N8501_config_t encoders_conf_TS5700N8501 =
{
  ENCODERS_VAR_UNINITIALIZED,
  {/*UART PINOUT*/
    {HW_UART_TX_PORT, HW_UART_TX_PIN},
	{HW_UART_RX_PORT, HW_UART_RX_PIN},
#ifdef HW_ADC_EXT_GPIO
	{HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN}
#else
	{ENCODERS_VAR_UNINITIALIZED, ENCODERS_VAR_UNINITIALIZED}
#endif
  },
  {/*UART CONFIG*/
    2500000,
	0,
	USART_CR2_LINEN,
	0
  }
};
#endif

