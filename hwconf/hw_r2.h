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
 * hw_r2.h
 *
 *  Created on: 14 apr 2014
 *      Author: benjamin
 */

#ifndef HW_R2_H_
#define HW_R2_H_

#define HW_NAME					"R2"

// Macros
#define ENABLE_GATE()			palSetPad(GPIOC, 9)
#define DISABLE_GATE()			palClearPad(GPIOC, 9)
#define DCCAL_ON()				palSetPad(GPIOB, 12)
#define DCCAL_OFF()				palClearPad(GPIOB, 12)
#define IS_DRV_FAULT()			(!palReadPad(GPIOC, 12))

#define LED_GREEN_ON()			palSetPad(GPIOB, 6)
#define LED_GREEN_OFF()			palClearPad(GPIOB, 6)
#define LED_RED_ON()			palSetPad(GPIOB, 7)
#define LED_RED_OFF()			palClearPad(GPIOB, 7)

/*
 * ADC Vector
 *
 * 0:	IN0		SENS3
 * 1:	IN1		SENS2
 * 2:	IN2		SENS1
 * 3:	IN5		CURR1
 * 4:	IN6		CURR2
 * 5:	IN10	TEMP_MOTOR
 * 6:	IN4		TEMP_PCB
 * 7:	IN9		TEMP_MOS_5
 * 8:	IN12	TEMP_MOS_2
 * 9:	Vrefint
 * 10:	IN15	ADC_EXT
 * 11:	IN11	TEMP_MOS_1
 * 12:	IN14	AN_IN
 * 13:	IN8		TEMP_MOS_4
 * 14:	IN13	TEMP_MOS_3
 */

#define HW_ADC_CHANNELS			15
#define HW_ADC_INJ_CHANNELS		2
#define HW_ADC_NBR_CONV			5

// ADC Indexes
#define ADC_IND_SENS1			2
#define ADC_IND_SENS2			1
#define ADC_IND_SENS3			0
#define ADC_IND_CURR1			3
#define ADC_IND_CURR2			4
#define ADC_IND_VIN_SENS		12
#define ADC_IND_EXT				10
#define ADC_IND_TEMP_MOS1		11
#define ADC_IND_TEMP_MOS2		8
#define ADC_IND_TEMP_MOS3		14
#define ADC_IND_TEMP_MOS4		13
#define ADC_IND_TEMP_MOS5		7
#define ADC_IND_TEMP_MOS6		7 // TODO! Same as MOS5
#define ADC_IND_TEMP_PCB		6
#define ADC_IND_TEMP_MOS		ADC_IND_TEMP_MOS2
#define ADC_IND_TEMP_MOTOR		5
#define ADC_IND_VREFINT			9

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					33000.0
#endif
#ifndef VIN_R2
#define VIN_R2					2200.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN		10.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES		0.001
#endif

// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
#define NTC_RES(adc_val)		(10000.0 / ((4095.0 / (float)adc_val) - 1.0))
#define NTC_TEMP(adc_ind)		(1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3434.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4096.0 * V_REG)

// Double samples in beginning and end for positive current measurement.
// Useful when the shunt sense traces have noise that causes offset.
#ifndef CURR1_DOUBLE_SAMPLE
#define CURR1_DOUBLE_SAMPLE	0
#endif
#ifndef CURR2_DOUBLE_SAMPLE
#define CURR2_DOUBLE_SAMPLE	0
#endif

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOC
#define HW_ADC_EXT_PIN			5

// UART Peripheral
#define HW_UART_DEV				SD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOC
#define HW_UART_TX_PIN			10
#define HW_UART_RX_PORT			GPIOC
#define HW_UART_RX_PIN			11

// ICU Peripheral for servo decoding
#define HW_ICU_TIMER			TIM3
#define HW_ICU_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE)
#define HW_ICU_DEV				ICUD3
#define HW_ICU_CHANNEL			ICU_CHANNEL_2
#define HW_ICU_GPIO_AF			GPIO_AF_TIM3
#define HW_ICU_GPIO				GPIOB
#define HW_ICU_PIN				5

// I2C Peripheral
#define HW_I2C_DEV				I2CD2
#define HW_I2C_GPIO_AF			GPIO_AF_I2C2
#define HW_I2C_SCL_PORT			GPIOB
#define HW_I2C_SCL_PIN			10
#define HW_I2C_SDA_PORT			GPIOB
#define HW_I2C_SDA_PIN			11

// Hall/encoder pins
//#define HW_HALL_ENC_GPIO1		GPIOC
//#define HW_HALL_ENC_PIN1		6
//#define HW_HALL_ENC_GPIO2		GPIOC
//#define HW_HALL_ENC_PIN2		7
//#define HW_HALL_ENC_GPIO3		GPIOC
//#define HW_HALL_ENC_PIN3		8
//#define HW_ENC_TIM				TIM3
//#define HW_ENC_TIM_AF			GPIO_AF_TIM3
//#define HW_ENC_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE)
//#define HW_ENC_EXTI_PORTSRC		EXTI_PortSourceGPIOC
//#define HW_ENC_EXTI_PINSRC		EXTI_PinSource8
//#define HW_ENC_EXTI_CH			EXTI9_5_IRQn
//#define HW_ENC_EXTI_LINE		EXTI_Line8
//#define HW_ENC_EXTI_ISR_VEC		EXTI9_5_IRQHandler
//#define HW_ENC_TIM_ISR_CH		TIM3_IRQn
//#define HW_ENC_TIM_ISR_VEC		TIM3_IRQHandler

// Hall/encoder pins
#define HW_HALL_ENC_GPIO1		GPIOC
#define HW_HALL_ENC_PIN1		6
#define HW_HALL_ENC_GPIO2		GPIOC
#define HW_HALL_ENC_PIN2		7
#define HW_HALL_ENC_GPIO3		GPIOC
#define HW_HALL_ENC_PIN3		8
// TODO: This is wrong and only here for the code to compile. Servo and ABI encoder can't work simultaneously
// on this hardware.
#define HW_ENC_TIM				TIM4
#define HW_ENC_TIM_AF			GPIO_AF_TIM4
#define HW_ENC_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ENC_EXTI_PORTSRC		EXTI_PortSourceGPIOC
#define HW_ENC_EXTI_PINSRC		EXTI_PinSource11
#define HW_ENC_EXTI_CH			EXTI15_10_IRQn
#define HW_ENC_EXTI_LINE		EXTI_Line11
#define HW_ENC_EXTI_ISR_VEC		EXTI15_10_IRQHandler
#define HW_ENC_TIM_ISR_CH		TIM4_IRQn
#define HW_ENC_TIM_ISR_VEC		TIM4_IRQHandler

// NRF pins
#define NRF_PORT_CSN			HW_ICU_GPIO
#define NRF_PIN_CSN				HW_ICU_PIN
#define NRF_PORT_SCK			GPIOC
#define NRF_PIN_SCK				5
#define NRF_PORT_MOSI			HW_I2C_SDA_PORT
#define NRF_PIN_MOSI			HW_I2C_SDA_PIN
#define NRF_PORT_MISO			HW_I2C_SCL_PORT
#define NRF_PIN_MISO			HW_I2C_SCL_PIN

// Measurement macros
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO				(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Setting limits
#define HW_LIM_CURRENT			-100.0, 100.0
#define HW_LIM_CURRENT_IN		-100.0, 100.0
#define HW_LIM_CURRENT_ABS		0.0, 150.0
#define HW_LIM_VIN				6.0, 57.0
#define HW_LIM_ERPM				-200e3, 200e3
#define HW_LIM_DUTY_MIN			0.0, 0.1
#define HW_LIM_DUTY_MAX			0.0, 0.95
#define HW_LIM_TEMP_FET			-40.0, 110.0

#endif /* HW_R2_H_ */
