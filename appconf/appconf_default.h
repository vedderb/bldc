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

#ifndef APPCONF_DEFAULT_H_
#define APPCONF_DEFAULT_H_

// Default app configuration
#ifndef APPCONF_CONTROLLER_ID
#define APPCONF_CONTROLLER_ID				0
#endif
#ifndef APPCONF_TIMEOUT_MSEC
#define APPCONF_TIMEOUT_MSEC				1000
#endif
#ifndef APPCONF_TIMEOUT_BRAKE_CURRENT
#define APPCONF_TIMEOUT_BRAKE_CURRENT		0.0
#endif
#ifndef APPCONF_SEND_CAN_STATUS
#define APPCONF_SEND_CAN_STATUS				false
#endif
#ifndef APPCONF_SEND_CAN_STATUS_RATE_HZ
#define APPCONF_SEND_CAN_STATUS_RATE_HZ		500
#endif
#ifndef APPCONF_CAN_BAUD_RATE
#define APPCONF_CAN_BAUD_RATE				CAN_BAUD_500K
#endif

// The default app is UART in case the UART port is used for
// firmware updates.
#ifndef APPCONF_APP_TO_USE
#define APPCONF_APP_TO_USE					APP_UART
#endif

// PPM app configureation
#ifndef APPCONF_PPM_CTRL_TYPE
#define APPCONF_PPM_CTRL_TYPE				PPM_CTRL_TYPE_NONE
#endif
#ifndef APPCONF_PPM_PID_MAX_ERPM
#define APPCONF_PPM_PID_MAX_ERPM			15000
#endif
#ifndef APPCONF_PPM_HYST
#define APPCONF_PPM_HYST					0.15
#endif
#ifndef APPCONF_PPM_PULSE_START
#define APPCONF_PPM_PULSE_START				1.0
#endif
#ifndef APPCONF_PPM_PULSE_END
#define APPCONF_PPM_PULSE_END				2.0
#endif
#ifndef APPCONF_PPM_PULSE_CENTER
#define APPCONF_PPM_PULSE_CENTER			1.5
#endif
#ifndef APPCONF_PPM_MEDIAN_FILTER
#define APPCONF_PPM_MEDIAN_FILTER			true
#endif
#ifndef APPCONF_PPM_SAFE_START
#define APPCONF_PPM_SAFE_START				true
#endif
#ifndef APPCONF_PPM_THROTTLE_EXP
#define APPCONF_PPM_THROTTLE_EXP			0.0
#endif
#ifndef APPCONF_PPM_THROTTLE_EXP_BRAKE
#define APPCONF_PPM_THROTTLE_EXP_BRAKE		0.0
#endif
#ifndef APPCONF_PPM_THROTTLE_EXP_MODE
#define APPCONF_PPM_THROTTLE_EXP_MODE		THR_EXP_POLY
#endif
#ifndef APPCONF_PPM_RAMP_TIME_POS
#define APPCONF_PPM_RAMP_TIME_POS			0.3
#endif
#ifndef APPCONF_PPM_RAMP_TIME_NEG
#define APPCONF_PPM_RAMP_TIME_NEG			0.1
#endif
#ifndef APPCONF_PPM_MULTI_ESC
#define APPCONF_PPM_MULTI_ESC				false
#endif
#ifndef APPCONF_PPM_TC
#define APPCONF_PPM_TC						false
#endif
#ifndef APPCONF_PPM_TC_MAX_DIFF
#define APPCONF_PPM_TC_MAX_DIFF				3000.0
#endif

// ADC app configureation
#ifndef APPCONF_ADC_CTRL_TYPE
#define APPCONF_ADC_CTRL_TYPE				ADC_CTRL_TYPE_NONE
#endif
#ifndef APPCONF_ADC_HYST
#define APPCONF_ADC_HYST					0.15
#endif
#ifndef APPCONF_ADC_VOLTAGE_START
#define APPCONF_ADC_VOLTAGE_START			0.9
#endif
#ifndef APPCONF_ADC_VOLTAGE_END
#define APPCONF_ADC_VOLTAGE_END				3.0
#endif
#ifndef APPCONF_ADC_VOLTAGE_CENTER
#define APPCONF_ADC_VOLTAGE_CENTER			2.0
#endif
#ifndef APPCONF_ADC_VOLTAGE2_START
#define APPCONF_ADC_VOLTAGE2_START			0.9
#endif
#ifndef APPCONF_ADC_VOLTAGE2_END
#define APPCONF_ADC_VOLTAGE2_END				3.0
#endif
#ifndef APPCONF_ADC_USE_FILTER
#define APPCONF_ADC_USE_FILTER				true
#endif
#ifndef APPCONF_ADC_SAFE_START
#define APPCONF_ADC_SAFE_START				true
#endif
#ifndef APPCONF_ADC_CC_BUTTON_INVERTED
#define APPCONF_ADC_CC_BUTTON_INVERTED		false
#endif
#ifndef APPCONF_ADC_REV_BUTTON_INVERTED
#define APPCONF_ADC_REV_BUTTON_INVERTED		false
#endif
#ifndef APPCONF_ADC_VOLTAGE_INVERTED
#define APPCONF_ADC_VOLTAGE_INVERTED		false
#endif
#ifndef APPCONF_ADC_VOLTAGE2_INVERTED
#define APPCONF_ADC_VOLTAGE2_INVERTED		false
#endif
#ifndef APPCONF_ADC_THROTTLE_EXP
#define APPCONF_ADC_THROTTLE_EXP			0.0
#endif
#ifndef APPCONF_ADC_THROTTLE_EXP_BRAKE
#define APPCONF_ADC_THROTTLE_EXP_BRAKE		0.0
#endif
#ifndef APPCONF_ADC_THROTTLE_EXP_MODE
#define APPCONF_ADC_THROTTLE_EXP_MODE		THR_EXP_POLY
#endif
#ifndef APPCONF_ADC_RAMP_TIME_POS
#define APPCONF_ADC_RAMP_TIME_POS			0.3
#endif
#ifndef APPCONF_ADC_RAMP_TIME_NEG
#define APPCONF_ADC_RAMP_TIME_NEG			0.1
#endif
#ifndef APPCONF_ADC_MULTI_ESC
#define APPCONF_ADC_MULTI_ESC				false
#endif
#ifndef APPCONF_ADC_TC
#define APPCONF_ADC_TC						false
#endif
#ifndef APPCONF_ADC_TC_MAX_DIFF
#define APPCONF_ADC_TC_MAX_DIFF				3000.0
#endif
#ifndef APPCONF_ADC_UPDATE_RATE_HZ
#define APPCONF_ADC_UPDATE_RATE_HZ			500
#endif

// UART app
#ifndef APPCONF_UART_BAUDRATE
#define APPCONF_UART_BAUDRATE				115200
#endif

// Nunchuk app
#ifndef APPCONF_CHUK_CTRL_TYPE
#define APPCONF_CHUK_CTRL_TYPE				CHUK_CTRL_TYPE_CURRENT
#endif
#ifndef APPCONF_CHUK_HYST
#define APPCONF_CHUK_HYST					0.15
#endif
#ifndef APPCONF_CHUK_RAMP_TIME_POS
#define APPCONF_CHUK_RAMP_TIME_POS			0.9
#endif
#ifndef APPCONF_CHUK_RAMP_TIME_NEG
#define APPCONF_CHUK_RAMP_TIME_NEG			0.3
#endif
#ifndef APPCONF_STICK_ERPM_PER_S_IN_CC
#define APPCONF_STICK_ERPM_PER_S_IN_CC		3000.0
#endif
#ifndef APPCONF_CHUK_THROTTLE_EXP
#define APPCONF_CHUK_THROTTLE_EXP			0.0
#endif
#ifndef APPCONF_CHUK_THROTTLE_EXP_BRAKE
#define APPCONF_CHUK_THROTTLE_EXP_BRAKE		0.0
#endif
#ifndef APPCONF_CHUK_THROTTLE_EXP_MODE
#define APPCONF_CHUK_THROTTLE_EXP_MODE		THR_EXP_POLY
#endif
#ifndef APPCONF_CHUK_MULTI_ESC
#define APPCONF_CHUK_MULTI_ESC				false
#endif
#ifndef APPCONF_CHUK_TC
#define APPCONF_CHUK_TC						false
#endif
#ifndef APPCONF_CHUK_TC_MAX_DIFF
#define APPCONF_CHUK_TC_MAX_DIFF			3000.0
#endif

// NRF app
#ifndef APPCONF_NRF_SPEED
#define APPCONF_NRF_SPEED					NRF_SPEED_2M
#endif
#ifndef APPCONF_NRF_POWER
#define APPCONF_NRF_POWER					NRF_POWER_0DBM
#endif
#ifndef APPCONF_NRF_CRC
#define APPCONF_NRF_CRC						NRF_CRC_1B
#endif
#ifndef APPCONF_NRF_RETR_DELAY
#define APPCONF_NRF_RETR_DELAY				NRF_RETR_DELAY_250US
#endif
#ifndef APPCONF_NRF_RETRIES
#define APPCONF_NRF_RETRIES					3
#endif
#ifndef APPCONF_NRF_CHANNEL
#define APPCONF_NRF_CHANNEL					76
#endif
#ifndef APPCONF_NRF_ADDR_B0
#define APPCONF_NRF_ADDR_B0					0xC6
#endif
#ifndef APPCONF_NRF_ADDR_B1
#define APPCONF_NRF_ADDR_B1					0xC7
#endif
#ifndef APPCONF_NRF_ADDR_B2
#define APPCONF_NRF_ADDR_B2					0x0
#endif
#ifndef APPCONF_NRF_SEND_CRC_ACK
#define APPCONF_NRF_SEND_CRC_ACK			true
#endif

#endif /* APPCONF_DEFAULT_H_ */
