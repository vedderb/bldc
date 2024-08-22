/*
	Copyright 2021 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HW_60V2_ALVA_CORE_H_
#define HW_60V2_ALVA_CORE_H_

#ifdef HW_60V2_ALVA_IS_MK2
#define HW_NAME					"60v2_alva_mk2"
#elif defined(HW_60V2_ALVA_IS_MK1)
#define HW_NAME					"60v2_alva_mk1"
#else
#define HW_NAME					"60v2_alva"
#endif

#if !(defined(HW_60V2_ALVA_IS_MK1) || defined(HW_60V2_ALVA_IS_MK2))
#define ALVA_V0_PPM
//#define ALVA_V0_ABI_ENC
#endif

// HW properties
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_SHUNTS
#define HW_HAS_PHASE_FILTERS

// Macros
#ifdef HW_60V2_ALVA_IS_MK2
#define LED_GREEN_GPIO			GPIOB
#define LED_GREEN_PIN			1
#define LED_RED_GPIO			GPIOB
#define LED_RED_PIN				0
#else
#define LED_GREEN_GPIO			GPIOB
#define LED_GREEN_PIN			3
#if defined(ALVA_V0_PPM) || defined(ALVA_V0_ABI_ENC)
#define LED_RED_GPIO			GPIOB
#define LED_RED_PIN				7
#else
#define LED_RED_GPIO			GPIOB
#define LED_RED_PIN				4
#endif
#endif

#define LED_GREEN_ON()			palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF()			palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON()			palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF()			palClearPad(LED_RED_GPIO, LED_RED_PIN)

#define PHASE_FILTER_GPIO		GPIOC
#define PHASE_FILTER_PIN		9
#define PHASE_FILTER_ON()		palSetPad(PHASE_FILTER_GPIO, PHASE_FILTER_PIN)
#define PHASE_FILTER_OFF()		palClearPad(PHASE_FILTER_GPIO, PHASE_FILTER_PIN)

#define AUX_GPIO				GPIOC
#define AUX_PIN					12
#define AUX_ON()				palSetPad(AUX_GPIO, AUX_PIN)
#define AUX_OFF()				palClearPad(AUX_GPIO, AUX_PIN)
#define AUX2_GPIO				GPIOC
#define AUX2_PIN				13
#define AUX2_ON()				palSetPad(AUX2_GPIO, AUX2_PIN)
#define AUX2_OFF()				palClearPad(AUX2_GPIO, AUX2_PIN)

#define CURRENT_FILTER_GPIO		GPIOD
#define CURRENT_FILTER_PIN		2
#define CURRENT_FILTER_ON()		palSetPad(CURRENT_FILTER_GPIO, CURRENT_FILTER_PIN)
#define CURRENT_FILTER_OFF()	palClearPad(CURRENT_FILTER_GPIO, CURRENT_FILTER_PIN)

// Sensor port voltage control
#define SENSOR_VOLTAGE_GPIO		GPIOA
#define SENSOR_VOLTAGE_PIN		15
#define SENSOR_PORT_5V()		palClearPad(SENSOR_VOLTAGE_GPIO, SENSOR_VOLTAGE_PIN)
#define SENSOR_PORT_3V3()		palSetPad(SENSOR_VOLTAGE_GPIO, SENSOR_VOLTAGE_PIN)

/*
 * ADC Vector
 *
 * 0  (1):	IN0		SENS1
 * 1  (2):	IN1		SENS2
 * 2  (3):	IN2		SENS3
 * 3  (1):	IN10	CURR1
 * 4  (2):	IN11	CURR2
 * 5  (3):	IN12	CURR3
 * 6  (1):	IN5		ADC_EXT1
 * 7  (2):	IN6		ADC_EXT2
 * 8  (3):	IN3		TEMP_MOS
 * 9  (1):	IN14	TEMP_MOTOR
 * 10 (2):	IN15	TEMP_MOTOR_S2
 * 11 (3):	IN13	AN_IN
 * 12 (1):	Vrefint
 * 13 (2):	IN0		SENS1
 * 14 (3):	IN1		SENS2
 * 15 (1):  IN8		TEMP_MOS_2
 * 16 (2):  IN9		TEMP_MOS_3
 * 17 (3):  IN3		TEMP_MOS
 */

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
#define ADC_IND_EXT				6
#define ADC_IND_EXT2			7
#define ADC_IND_TEMP_MOS		8
#define ADC_IND_TEMP_MOTOR		9
#define ADC_IND_TEMP_MOTOR_S2	10
#define ADC_IND_VREFINT			12

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					56000.0
#endif
#ifndef VIN_R2
#define VIN_R2					2200.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN		20.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES		0.0005
#endif

// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
#define NTC_RES(adc_val)		((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind)		(1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	alva_temp_motor_max(beta)
#define TEMP_MOTOR_1(beta)		(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)
#define TEMP_MOTOR_2(beta)		(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR_S2]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4096.0 * V_REG)

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOA
#define HW_ADC_EXT_PIN			5
#define HW_ADC_EXT2_GPIO		GPIOA
#define HW_ADC_EXT2_PIN			6

// UART Peripheral
#define HW_UART_DEV				SD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOB
#define HW_UART_TX_PIN			10
#define HW_UART_RX_PORT			GPIOB
#define HW_UART_RX_PIN			11

// ICU Peripheral for servo decoding
#ifdef ALVA_V0_ABI_ENC
#define HW_USE_SERVO_TIM4
#define HW_ICU_TIMER			TIM4
#define HW_ICU_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ICU_DEV				ICUD4
#define HW_ICU_CHANNEL			ICU_CHANNEL_1
#define HW_ICU_GPIO_AF			GPIO_AF_TIM4
#define HW_ICU_GPIO				GPIOD
#define HW_ICU_PIN				12
#elif defined(ALVA_V0_PPM)
#define HW_ICU_TIMER			TIM3
#define HW_ICU_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ICU_DEV				ICUD3
#define HW_ICU_CHANNEL			ICU_CHANNEL_1
#define HW_ICU_GPIO_AF			GPIO_AF_TIM3
#define HW_ICU_GPIO				GPIOB
#define HW_ICU_PIN				4
#else
#define HW_USE_SERVO_TIM4
#define HW_ICU_TIMER			TIM4
#define HW_ICU_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ICU_DEV				ICUD4
#define HW_ICU_CHANNEL			ICU_CHANNEL_2
#define HW_ICU_GPIO_AF			GPIO_AF_TIM4
#define HW_ICU_GPIO				GPIOB
#define HW_ICU_PIN				7
#endif

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

// The first version has the same timer for both the encoder and PPM input. This option disables
// the encoder and enables PPM input.
#ifdef ALVA_V0_PPM
#undef HW_ENC_TIM
#undef HW_ENC_TIM_AF
#undef HW_ENC_TIM_CLK_EN
#undef HW_ENC_TIM_ISR_CH
#undef HW_ENC_TIM_ISR_VEC
#define HW_ENC_TIM				TIM4
#define HW_ENC_TIM_AF			GPIO_AF_TIM4
#define HW_ENC_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ENC_TIM_ISR_CH		TIM4_IRQn
#define HW_ENC_TIM_ISR_VEC		TIM4_IRQHandler
#endif

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

// BMI160
#ifdef HW_60V2_ALVA_IS_MK2
#define BMI160_SPI_PORT_NSS		GPIOA
#define BMI160_SPI_PIN_NSS		15
#define BMI160_SPI_PORT_SCK		GPIOB
#define BMI160_SPI_PIN_SCK		3
#define BMI160_SPI_PORT_MOSI	GPIOC
#define BMI160_SPI_PIN_MOSI		12
#define BMI160_SPI_PORT_MISO	GPIOB
#define BMI160_SPI_PIN_MISO		4
#define IMU_FLIP
#define IMU_ROT_90
#else
#define BMI160_SDA_GPIO			GPIOC
#define BMI160_SDA_PIN			11
#define BMI160_SCL_GPIO			GPIOC
#define BMI160_SCL_PIN			10
#define IMU_FLIP
#define IMU_ROT_180
#endif

// Second redundant CAN-port
#define HW_CAN2_RX_PORT			GPIOB
#define HW_CAN2_RX_PIN			5
#define HW_CAN2_TX_PORT			GPIOB
#define HW_CAN2_TX_PIN			6
#define HW_CAN2_GPIO_AF			GPIO_AF_CAN2
#define HW_CAN2_DEV				CAND2

// Measurement macros
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO				(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Override dead time
#define HW_DEAD_TIME_NSEC		500.0

// Default setting overrides
#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC
#endif
#ifndef MCCONF_FOC_F_ZV
#define MCCONF_FOC_F_ZV					30000.0
#endif
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT		150.0	// The maximum absolute current above which a fault is generated
#endif
#ifndef MCCONF_FOC_SAMPLE_V0_V7
#define MCCONF_FOC_SAMPLE_V0_V7			false	// Run control loop in both v0 and v7 (requires phase shunts)
#endif

// Setting limits
#define HW_LIM_CURRENT			-120.0, 120.0
#define HW_LIM_CURRENT_IN		-120.0, 120.0
#define HW_LIM_CURRENT_ABS		0.0, 160.0
#define HW_LIM_VIN				6.0, 72.0
#define HW_LIM_ERPM				-200e3, 200e3
#define HW_LIM_DUTY_MIN			0.0, 0.1
#define HW_LIM_DUTY_MAX			0.0, 0.99
#define HW_LIM_TEMP_FET			-40.0, 110.0

// HW Functions
float alva_temp_motor_max(float beta);

#endif /* HW_60V2_ALVA_CORE_H_ */
