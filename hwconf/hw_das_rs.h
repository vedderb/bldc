/*
	Copyright 2012-2016 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HW_DAS_RS_H_
#define HW_DAS_RS_H_

#define HW_NAME					"DAS_RS"

// HW properties
#define HW_HAS_DRV8313
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_SHUNTS

// Macros
#define ENABLE_GATE()			palSetPad(GPIOB, 5)
#define DISABLE_GATE()			palClearPad(GPIOB, 5)
#define DCCAL_ON()
#define DCCAL_OFF()
#define IS_DRV_FAULT()			(!palReadPad(GPIOD, 2))

#define LED_GREEN_ON()			palSetPad(GPIOB, 0)
#define LED_GREEN_OFF()			palClearPad(GPIOB, 0)
#define LED_RED_ON()			palSetPad(GPIOB, 1)
#define LED_RED_OFF()			palClearPad(GPIOB, 1)

// For power stages with enable pins (e.g. DRV8313)
#define ENABLE_BR1()			palSetPad(GPIOB, 13)
#define ENABLE_BR2()			palSetPad(GPIOB, 14)
#define ENABLE_BR3()			palSetPad(GPIOB, 15)
#define DISABLE_BR1()			palClearPad(GPIOB, 13)
#define DISABLE_BR2()			palClearPad(GPIOB, 14)
#define DISABLE_BR3()			palClearPad(GPIOB, 15)
#define ENABLE_BR()				palWriteGroup(GPIOB, PAL_GROUP_MASK(3), 13, 7)
#define DISABLE_BR()			palWriteGroup(GPIOB, PAL_GROUP_MASK(3), 13, 0)

#define INIT_BR()				palSetPadMode(GPIOB, 13, \
								PAL_MODE_OUTPUT_PUSHPULL | \
								PAL_STM32_OSPEED_HIGHEST); \
								palSetPadMode(GPIOB, 14, \
								PAL_MODE_OUTPUT_PUSHPULL | \
								PAL_STM32_OSPEED_HIGHEST); \
								palSetPadMode(GPIOB, 15, \
								PAL_MODE_OUTPUT_PUSHPULL | \
								PAL_STM32_OSPEED_HIGHEST); \
								DISABLE_BR();

/*
 * ADC Vector
 *
 * 0:	IN0		SENS1
 * 1:	IN1		SENS2
 * 2:	IN2		SENS3
 * 3:	IN10	CURR1
 * 4:	IN11	CURR2
 * 5:	IN12	CURR3
 * 6:	IN5		ADC_EXT1
 * 7:	IN6		ADC_EXT2
 * 8:	IN3		TEMP_PCB
 * 9:	IN14	TEMP_MOTOR
 * 10:	IN15	ADC_EXT3
 * 11:	IN13	AN_IN
 * 12:	Vrefint
 * 13:	IN0		SENS1
 * 14:	IN1		SENS2
 */

#define HW_ADC_CHANNELS			15
#define HW_ADC_INJ_CHANNELS		3
#define HW_ADC_NBR_CONV			5

// ADC Indexes
#define ADC_IND_SENS1			0
#define ADC_IND_SENS2			1
#define ADC_IND_SENS3			2
#define ADC_IND_CURR1			3
#define ADC_IND_CURR2			4
#define ADC_IND_CURR3			5
#define ADC_IND_VIN_SENS		11
#define ADC_IND_EXT				6
#define ADC_IND_TEMP_MOS		8
#define ADC_IND_TEMP_MOTOR		9
#define ADC_IND_VREFINT			12

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					39000.0
#endif
#ifndef VIN_R2
#define VIN_R2					2200.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN		20.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES		0.005
#endif

// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
#define NTC_RES(adc_val)		((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind)		(1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3434.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4096.0 * V_REG)

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

// Number of servo outputs
#define HW_SERVO_NUM			2

// UART Peripheral
#define HW_UART_DEV				UARTD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOB
#define HW_UART_TX_PIN			10
#define HW_UART_RX_PORT			GPIOB
#define HW_UART_RX_PIN			11

// ICU Peripheral for servo decoding
#define HW_ICU_DEV				ICUD4
#define HW_ICU_CHANNEL			ICU_CHANNEL_1
#define HW_ICU_GPIO_AF			GPIO_AF_TIM4
#define HW_ICU_GPIO				GPIOB
#define HW_ICU_PIN				6

// I2C Peripheral
#define HW_I2C_DEV				I2CD2
#define HW_I2C_GPIO_AF			GPIO_AF_I2C2
#define HW_I2C_SCL_PORT			GPIOB
#define HW_I2C_SCL_PIN			10
#define HW_I2C_SDA_PORT			GPIOB
#define HW_I2C_SDA_PIN			11

// Hall/encoder pins
#define HW_HALL_ENC_GPIO1		GPIOC
#define HW_HALL_ENC_PIN1		7
#define HW_HALL_ENC_GPIO2		GPIOC
#define HW_HALL_ENC_PIN2		8
#define HW_HALL_ENC_GPIO3		GPIOC
#define HW_HALL_ENC_PIN3		6
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

// NRF pins
#define NRF_PORT_CSN			GPIOB
#define NRF_PIN_CSN				12
#define NRF_PORT_SCK			GPIOB
#define NRF_PIN_SCK				4
#define NRF_PORT_MOSI			GPIOB
#define NRF_PIN_MOSI			3
#define NRF_PORT_MISO			GPIOD
#define NRF_PIN_MISO			2

// SPI pins
#define HW_SPI_DEV				SPID1
#define HW_SPI_GPIO_AF			GPIO_AF_SPI1
#define HW_SPI_PORT_NSS			GPIOA
#define HW_SPI_PIN_NSS			4
#define HW_SPI_PORT_SCK			GPIOA
#define HW_SPI_PIN_SCK			5
#define HW_SPI_PORT_MOSI		GPIOA
#define HW_SPI_PIN_MOSI			7
#define HW_SPI_PORT_MISO		GPIOA
#define HW_SPI_PIN_MISO			6

// Measurement macros
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO				(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Default motor configuration values for this HW
#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE			MOTOR_TYPE_FOC
#endif

#ifndef MCCONF_L_CURRENT_MAX
#define MCCONF_L_CURRENT_MAX				2.0	// Current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_CURRENT_MIN
#define MCCONF_L_CURRENT_MIN				-2.0	// Current limit in Amperes (Lower)
#endif
#ifndef MCCONF_L_IN_CURRENT_MAX
#define MCCONF_L_IN_CURRENT_MAX				2.0	// Input current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_IN_CURRENT_MIN
#define MCCONF_L_IN_CURRENT_MIN				-2.0	// Input current limit in Amperes (Lower)
#endif
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT			10.0	// The maximum absolute current above which a fault is generated
#endif

#ifndef MCCONF_CC_MIN_CURRENT
#define MCCONF_CC_MIN_CURRENT				0.01	// Minimum allowed current
#endif

// FOC settings
#ifndef MCCONF_FOC_CURRENT_KP
#define MCCONF_FOC_CURRENT_KP				4.8
#endif
#ifndef MCCONF_FOC_CURRENT_KI
#define MCCONF_FOC_CURRENT_KI				5800.0
#endif
#ifndef MCCONF_FOC_F_SW
#define MCCONF_FOC_F_SW						20000.0
#endif
#ifndef MCCONF_FOC_MOTOR_L
#define MCCONF_FOC_MOTOR_L					0.0048
#endif
#ifndef MCCONF_FOC_MOTOR_R
#define MCCONF_FOC_MOTOR_R					5.8
#endif
#ifndef MCCONF_FOC_MOTOR_FLUX_LINKAGE
#define MCCONF_FOC_MOTOR_FLUX_LINKAGE		0.023
#endif
#ifndef MCCONF_FOC_OBSERVER_GAIN
#define MCCONF_FOC_OBSERVER_GAIN			31e4
#endif
#ifndef MCCONF_FOC_OPENLOOP_RPM
#define MCCONF_FOC_OPENLOOP_RPM				500.0
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_HYST
#define MCCONF_FOC_SL_OPENLOOP_HYST			0.5
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_TIME
#define MCCONF_FOC_SL_OPENLOOP_TIME			0.3
#endif

#endif /* HW_DAS_RS_H_ */
