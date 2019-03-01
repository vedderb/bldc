/*
	Copyright 2012 - 2019 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HW_H_
#define HW_H_

#include "conf_general.h"
#include "stm32f4xx_conf.h"

#include HW_HEADER

#ifndef HW_NAME
#error "No hardware name set"
#endif

// Default macros in case there is no hardware support or no need to change them.

#ifndef ENABLE_GATE
#define ENABLE_GATE()
#endif
#ifndef DISABLE_GATE
#define DISABLE_GATE()
#endif
#ifndef DCCAL_ON
#define DCCAL_ON()
#endif
#ifndef DCCAL_OFF
#define DCCAL_OFF()
#endif
#ifndef IS_DRV_FAULT
#define IS_DRV_FAULT()			0
#endif

#ifndef AUX_ON
#define AUX_ON()
#endif
#ifndef AUX_OFF
#define AUX_OFF()
#endif

#ifndef PHASE_FILTER_ON
#define PHASE_FILTER_ON()
#endif
#ifndef PHASE_FILTER_OFF
#define PHASE_FILTER_OFF()
#endif

#ifndef CURRENT_FILTER_ON
#define CURRENT_FILTER_ON()
#endif
#ifndef CURRENT_FILTER_OFF
#define CURRENT_FILTER_OFF()
#endif

// Individual MOSFET temperature sensors. Override if available.
#ifndef NTC_TEMP_MOS1
#define NTC_TEMP_MOS1()		0.0
#endif
#ifndef NTC_TEMP_MOS2
#define NTC_TEMP_MOS2()		0.0
#endif
#ifndef NTC_TEMP_MOS3
#define NTC_TEMP_MOS3()		0.0
#endif

// Current ADC macros. Override them for custom current measurement functions.
#ifndef GET_CURRENT1
#ifdef INVERTED_SHUNT_POLARITY
#define GET_CURRENT1()		(4095 - ADC_Value[ADC_IND_CURR1])
#else
#define GET_CURRENT1()		ADC_Value[ADC_IND_CURR1]
#endif
#endif
#ifndef GET_CURRENT2
#ifdef INVERTED_SHUNT_POLARITY
#define GET_CURRENT2()		(4095 - ADC_Value[ADC_IND_CURR2])
#else
#define GET_CURRENT2()		ADC_Value[ADC_IND_CURR2]
#endif
#endif
#ifndef GET_CURRENT3
#ifdef INVERTED_SHUNT_POLARITY
#define GET_CURRENT3()		(4095 - ADC_Value[ADC_IND_CURR3])
#else
#define GET_CURRENT3()		ADC_Value[ADC_IND_CURR3]
#endif
#endif

// NRF SW SPI (default to spi header pins)
#ifndef NRF_PORT_CSN
#define NRF_PORT_CSN			HW_SPI_PORT_NSS
#endif
#ifndef NRF_PIN_CSN
#define NRF_PIN_CSN				HW_SPI_PIN_NSS
#endif
#ifndef NRF_PORT_SCK
#define NRF_PORT_SCK			HW_SPI_PORT_SCK
#endif
#ifndef NRF_PIN_SCK
#define NRF_PIN_SCK				HW_SPI_PIN_SCK
#endif
#ifndef NRF_PORT_MOSI
#define NRF_PORT_MOSI			HW_SPI_PORT_MOSI
#endif
#ifndef NRF_PIN_MOSI
#define NRF_PIN_MOSI			HW_SPI_PIN_MOSI
#endif
#ifndef NRF_PORT_MISO
#define NRF_PORT_MISO			HW_SPI_PORT_MISO
#endif
#ifndef NRF_PIN_MISO
#define NRF_PIN_MISO			HW_SPI_PIN_MISO
#endif

// CAN device and port (default CAN1)
#ifndef HW_CANH_PORT
#define HW_CANH_PORT			GPIOB
#endif
#ifndef HW_CANH_PIN
#define HW_CANH_PIN				8
#endif
#ifndef HW_CANL_PORT
#define HW_CANL_PORT			GPIOB
#endif
#ifndef HW_CANL_PIN
#define HW_CANL_PIN				9
#endif
#ifndef HW_CAN_GPIO_AF
#define HW_CAN_GPIO_AF			GPIO_AF_CAN1
#endif
#ifndef HW_CAN_DEV
#define HW_CAN_DEV				CAND1
#endif

// Hook to call when trying to initialize the permanent NRF failed. Can be
// used to e.g. reconfigure pins.
#ifndef HW_PERMANENT_NRF_FAILED_HOOK
#define HW_PERMANENT_NRF_FAILED_HOOK()
#endif

// Default ID
#ifndef HW_DEFAULT_ID
#define HW_DEFAULT_ID			(APPCONF_CONTROLLER_ID >= 0 ? APPCONF_CONTROLLER_ID : hw_id_from_uuid())
#endif

#ifndef HW_LIM_CURRENT
#define HW_LIM_CURRENT			-100.0, 100.0
#endif
#ifndef HW_LIM_CURRENT_ABS
#define HW_LIM_CURRENT_ABS		0.0, 140.0
#endif

#ifndef HW_LIM_FOC_CTRL_LOOP_FREQ
#define HW_LIM_FOC_CTRL_LOOP_FREQ	3000.0, 30000.0
#endif

// Functions
void hw_init_gpio(void);
void hw_setup_adc_channels(void);
void hw_start_i2c(void);
void hw_stop_i2c(void);
void hw_try_restore_i2c(void);
uint8_t hw_id_from_uuid(void);

#endif /* HW_H_ */
