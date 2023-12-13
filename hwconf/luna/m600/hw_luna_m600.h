/*
	Copyright 2020 Marcos Chaparro	mchaparro@powerdesigns.ca
	Copyright 2018 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HW_LUNA_M600_H_
#define HW_LUNA_M600_H_

#define FW_NAME					"2023.12.13"

#ifdef M600_60V_BATTERY
#include "mcconf_luna_m600_60V.h"
#else
#include "mcconf_luna_m600_48V.h"
#endif

#include "appconf_luna_m600.h"

#define QMLUI_SOURCE_HW		"hwconf/luna/m600/qmlui_luna_m600.c"
#define QMLUI_HEADER_HW		"hwconf/luna/m600/qmlui_luna_m600.h"
#define QMLUI_HW_FULLSCREEN

// HW properties
#define HW_HAS_GATE_DRIVER_SUPPLY_MONITOR
#define HW_HAS_WHEEL_SPEED_SENSOR
#define HW_HAS_PAS_TORQUE_SENSOR
#define HW_HAS_LUNA_CANBUS_DISPLAY
#define HW_USE_BRK

#ifdef M600_Rev5
#ifdef M600_60V_BATTERY
#define HW_NAME					"LUNA_M600_V2_Rev5_60V"
#else
#define HW_NAME					"LUNA_M600_V2_Rev5"
#endif
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_FILTERS
#define HW_PHASE_A_FILTER_GPIO		GPIOB
#define HW_PHASE_A_FILTER_PIN		5
#define HW_PHASE_B_FILTER_GPIO		GPIOB
#define HW_PHASE_B_FILTER_PIN		4
#define HW_PHASE_C_FILTER_GPIO		GPIOD
#define HW_PHASE_C_FILTER_PIN		2
#define PHASE_FILTER_ON()		palClearPad(HW_PHASE_A_FILTER_GPIO, HW_PHASE_A_FILTER_PIN); \
								palClearPad(HW_PHASE_B_FILTER_GPIO, HW_PHASE_B_FILTER_PIN); \
								palClearPad(HW_PHASE_C_FILTER_GPIO, HW_PHASE_C_FILTER_PIN)
#define PHASE_FILTER_OFF()		palSetPad(HW_PHASE_A_FILTER_GPIO, HW_PHASE_A_FILTER_PIN); \
								palSetPad(HW_PHASE_B_FILTER_GPIO, HW_PHASE_B_FILTER_PIN); \
								palSetPad(HW_PHASE_C_FILTER_GPIO, HW_PHASE_C_FILTER_PIN)

#else
#define HW_NAME					"LUNA_M600_V2"
#define HW_HAS_PHASE_SHUNTS
// Some batches had the phase C shunt amplifier DNP, so this fills the gap
// Ia + Ib + Ic = 0   so Ic = -Ia - Ib
#define HW_HAS_3_SHUNTS
#define GET_CURRENT3()			(-(GET_CURRENT1() - 2048.0 + GET_CURRENT2() -2048.0) + 2048.0)
#endif

// Macros
#define LED_GREEN_GPIO			GPIOB
#define LED_GREEN_PIN			2
#define LED_RED_GPIO			GPIOB
#define LED_RED_PIN				11

#define LED_GREEN_ON()			palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF()			palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON()			palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF()			palClearPad(LED_RED_GPIO, LED_RED_PIN)

#define AUX_GPIO				GPIOC
#define AUX_PIN					14
#define AUX_ON()				palSetPad(AUX_GPIO, AUX_PIN)
#define AUX_OFF()				palClearPad(AUX_GPIO, AUX_PIN)

#define MT6816_PROG_EN_GPIO		GPIOD
#define MT6816_PROG_EN_PIN		2
#define MT6816_PROG_EN()		palSetPad(MT6816_PROG_EN_GPIO, MT6816_PROG_EN_PIN)
#define MT6816_PROG_DISABLE()	palClearPad(MT6816_PROG_EN_GPIO, MT6816_PROG_EN_PIN)

#define CURRENT_FILTER_ON()
#define CURRENT_FILTER_OFF()

#define BRK_GPIO				GPIOB
#define BRK_PIN					12

// Shutdown pin
#define HW_SHUTDOWN_GPIO		GPIOC
#define HW_SHUTDOWN_PIN			15
#define HW_SHUTDOWN_HOLD_ON()	palSetPad(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN)
#define HW_SHUTDOWN_HOLD_OFF()	palClearPad(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN)
#define HW_SAMPLE_SHUTDOWN()	!hw_luna_m600_shutdown_button_down()

#define HW_FORCE_SHUTDOWN_GPIO  GPIOC
#define HW_FORCE_SHUTDOWN_PIN   13
#define HW_FORCE_SHUTDOWN_OFF()	palClearPad(HW_FORCE_SHUTDOWN_GPIO, HW_FORCE_SHUTDOWN_PIN)
#define HW_FORCE_SHUTDOWN()		palSetPad(HW_FORCE_SHUTDOWN_GPIO, HW_FORCE_SHUTDOWN_PIN)

// Hold shutdown pin early to wake up on short pulses
#define HW_EARLY_INIT()			palSetPadMode(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN, PAL_MODE_OUTPUT_PUSHPULL); \
								HW_SHUTDOWN_HOLD_ON(); \
								HW_FORCE_SHUTDOWN_OFF(); \
								palSetPadMode(HW_FORCE_SHUTDOWN_GPIO, HW_FORCE_SHUTDOWN_PIN, PAL_MODE_OUTPUT_PUSHPULL); \
								HW_FORCE_SHUTDOWN_OFF();

#define HW_ADC_CHANNELS			18
#define HW_ADC_INJ_CHANNELS		3
#define HW_ADC_NBR_CONV			6

// ADC Indexes
#define ADC_IND_SENS1			0
#define ADC_IND_SENS2			1
#define ADC_IND_SENS3			2
#define ADC_IND_CURR1			3
#define ADC_IND_CURR2			4
#define ADC_IND_CURR3			5
#define ADC_IND_VIN_SENS		11
#define ADC_IND_VOUT_GATE_DRV	12
#define ADC_IND_EXT				10
#define ADC_IND_EXT2			6
#define ADC_IND_EXT3			13
#define ADC_IND_TEMP_MOS		15
#define ADC_IND_ON_OFF_BUTTON	8
#define ADC_IND_TEMP_MOS_3		16
#define ADC_IND_TEMP_MOTOR		9
#define ADC_IND_VREFINT			16

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					66500.0
#endif
#ifndef VIN_R2
#define VIN_R2					2000.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN		20.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES		(0.0005 / 2.0)
#endif

// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// 12V supply voltage
#define GET_GATE_DRIVER_SUPPLY_VOLTAGE()	((float)ADC_VOLTS(ADC_IND_VOUT_GATE_DRV) * 11.0)

// ON-OFF button sense
#define GET_ON_OFF_BUTTON_VOLTAGE()			((float)ADC_VOLTS(ADC_IND_ON_OFF_BUTTON) * 4.57)

// NTC Termistors
#define NTC_RES(adc_val)		(10000.0 * adc_val / ( 4095.0 - adc_val))
#define NTC_TEMP(adc_ind)		hw_get_mosfet_temp_filtered()
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)
#define PTC_TEMP_MOTOR(res, con, tbase)			(((NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) - res) / NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR])) * 100.0 / con - 10.0)
#define PTC_TEMP_MOTOR_2(res, con, tbase)		0.0
#define MOTOR_TEMP_LPF	0.001

#define NTC_RES_MOTOR(adc_val)	(1000.0 / ((4095.0 / (float)adc_val) - 1.0))

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4096.0 * V_REG)
                                
// Use these temperature channels for extra logging insight
//log torque sensor data
#define NTC_TEMP_MOS1()         hw_get_PAS_torque()
//log throttle data
#define NTC_TEMP_MOS2()         ((float)ADC_VOLTS(ADC_IND_EXT))
// log gate driver supply voltage
//#define NTC_TEMP_MOS3()         GET_GATE_DRIVER_SUPPLY_VOLTAGE()
#define NTC_TEMP_MOS3()         hw_get_encoder_error()

// Double samples in beginning and end for positive current measurement.
// Useful when the shunt sense traces have noise that causes offset.
#ifndef CURR1_DOUBLE_SAMPLE
#define CURR1_DOUBLE_SAMPLE		0
#endif
#ifndef CURR2_DOUBLE_SAMPLE
#define CURR2_DOUBLE_SAMPLE		0
#endif
#ifndef CURR3_DOUBLE_SAMPLE
#define CURR3_DOUBLE_SAMPLE		0
#endif

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOC
#define HW_ADC_EXT_PIN			5
#define HW_ADC_EXT2_GPIO		GPIOB
#define HW_ADC_EXT2_PIN			0

// UART Peripheral
#define HW_UART_DEV				SD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOD
#define HW_UART_TX_PIN			8
#define HW_UART_RX_PORT			GPIOD
#define HW_UART_RX_PIN			9

// Permanent UART Peripheral (for NRF51)
#define HW_UART_P_BAUD			115200
#define HW_UART_P_DEV			SD1
#define HW_UART_P_GPIO_AF		GPIO_AF_USART1
#define HW_UART_P_TX_PORT		GPIOB
#define HW_UART_P_TX_PIN		6
#define HW_UART_P_RX_PORT		GPIOB
#define HW_UART_P_RX_PIN		7

// NRF SWD
#ifdef M600_Rev5
#define NRF5x_SWDIO_GPIO		GPIOB
#define NRF5x_SWDIO_PIN			3
#define NRF5x_SWCLK_GPIO		GPIOA
#define NRF5x_SWCLK_PIN			15
#else
#define NRF5x_SWDIO_GPIO		GPIOA
#define NRF5x_SWDIO_PIN			15
#define NRF5x_SWCLK_GPIO		GPIOB
#define NRF5x_SWCLK_PIN			3
#endif

// ICU Peripheral for servo decoding. Not used, routed to a pin not present in 64 pin
// package to free USART1 TX pad
#define HW_USE_SERVO_TIM4
#define HW_ICU_TIMER			TIM4
#define HW_ICU_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ICU_DEV				ICUD4
#define HW_ICU_CHANNEL			ICU_CHANNEL_1
#define HW_ICU_GPIO_AF			GPIO_AF_TIM4
#define HW_ICU_GPIO				GPIOD
#define HW_ICU_PIN				12

// I2C Peripheral
#define HW_I2C_DEV				I2CD2
#define HW_I2C_GPIO_AF			GPIO_AF_I2C2
#define HW_I2C_SCL_PORT			GPIOB
#define HW_I2C_SCL_PIN			10
#define HW_I2C_SDA_PORT			GPIOB
#define HW_I2C_SDA_PIN			11

// Hall/encoder pins
#define HW_HALL_ENC_GPIO1		GPIOC
#define HW_HALL_ENC_PIN1		6
#define HW_HALL_ENC_GPIO2		GPIOC
#define HW_HALL_ENC_PIN2		7
#define HW_HALL_ENC_GPIO3		GPIOC
#define HW_HALL_ENC_PIN3		8
#define HW_ENC_TIM				TIM3
#define HW_ENC_TIM_AF			GPIO_AF_TIM3
#define HW_ENC_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE)
#define HW_ENC_EXTI_PORTSRC		EXTI_PortSourceGPIOC
#define HW_ENC_EXTI_PINSRC		EXTI_PinSource8
#define HW_ENC_EXTI_CH			EXTI9_5_IRQn
#define HW_ENC_EXTI_LINE		EXTI_Line8
#define HW_ENC_EXTI_ISR_VEC		EXTI9_5_IRQHandler
#define HW_ENC_TIM_ISR_CH		TIM3_IRQn
#define HW_ENC_TIM_ISR_VEC		TIM3_IRQHandler

// SPI pins
#define HW_SPI_DEV				SPID3
#define HW_SPI_GPIO_AF			GPIO_AF_SPI3
#define HW_SPI_PORT_NSS			GPIOA
#define HW_SPI_PIN_NSS			4
#define HW_SPI_PORT_SCK			GPIOC
#define HW_SPI_PIN_SCK			10
#define HW_SPI_PORT_MOSI		GPIOC
#define HW_SPI_PIN_MOSI			12
#define HW_SPI_PORT_MISO		GPIOC
#define HW_SPI_PIN_MISO			11

// Pedal Assist pins
#define HW_PAS1_PORT			GPIOA
#define HW_PAS1_PIN				6
#define HW_PAS2_PORT			GPIOA
#define HW_PAS2_PIN				7

#ifdef HW_HAS_WHEEL_SPEED_SENSOR
#define HW_SPEED_SENSOR_PORT	GPIOC
#define HW_SPEED_SENSOR_PIN		9
#endif

// Measurement macros
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO				(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Override dead time.
#define HW_DEAD_TIME_NSEC		460.0

// Default setting overrides
#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE			85.0	// Maximum input voltage
#endif
#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC
#endif
#ifndef MCCONF_FOC_F_SW
#define MCCONF_FOC_F_SW					30000.0
#endif
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT		250.0	// The maximum absolute current above which a fault is generated
#endif
#ifndef MCCONF_FOC_SAMPLE_V0_V7
#define MCCONF_FOC_SAMPLE_V0_V7			false	// Run control loop in both v0 and v7 (requires phase shunts)
#endif
#ifndef MCCONF_L_IN_CURRENT_MAX
#define MCCONF_L_IN_CURRENT_MAX			150.0	// Input current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_IN_CURRENT_MIN
#define MCCONF_L_IN_CURRENT_MIN			-150.0	// Input current limit in Amperes (Lower)
#endif

// Setting limits
#define HW_LIM_CURRENT			-100.0, 100.0
#define HW_LIM_CURRENT_IN		-60.0, 60.0
#define HW_LIM_CURRENT_ABS		0.0, 200.0
#define HW_LIM_VIN				6.0, 86.0
#define HW_LIM_ERPM				-200e3, 200e3
#define HW_LIM_DUTY_MIN			0.0, 0.1
#define HW_LIM_DUTY_MAX			0.0, 0.99
#define HW_LIM_TEMP_FET			-40.0, 95.0

#define HW_GATE_DRIVER_SUPPLY_MIN_VOLTAGE	11.0
#define HW_GATE_DRIVER_SUPPLY_MAX_VOLTAGE	13.6

// HW-specific functions
void hw_update_speed_sensor(void);
float hw_get_speed(void);
float hw_get_distance(void);
float hw_get_distance_abs(void);
float hw_get_mosfet_temp_filtered(void);
bool hw_luna_m600_shutdown_button_down(void);
bool hw_luna_m600_minus_button_down(void);
float hw_get_PAS_torque(void);
bool hw_m600_has_fixed_throttle_level(void);
void hw_recover_encoder_offset(void);
float hw_get_encoder_error(void);

#endif /* HW_LUNA_M600_H_ */
