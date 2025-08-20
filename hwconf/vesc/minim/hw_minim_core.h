/*
	Copyright 2023 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HW_MINIM_CORE_H_
#define HW_MINIM_CORE_H_

#ifdef HW_MINIM
  #define HW_NAME			"Minim"
#else
  #error "Must define hardware type"
#endif

// HW properties
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_FILTERS
#define INVERTED_SHUNT_POLARITY

// Macros
#define LED_GREEN_GPIO			GPIOB
#define LED_GREEN_PIN			7
#define LED_RED_GPIO			GPIOB
#define LED_RED_PIN				5

#define LED_GREEN_ON()			palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF()			palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON()			palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF()			palClearPad(LED_RED_GPIO, LED_RED_PIN)

// Phase filter
#define PHASE_FILTER_OFF()			palSetPad(GPIOC, 13); palSetPad(GPIOC, 14); palSetPad(GPIOC, 15)
#define PHASE_FILTER_ON()			palClearPad(GPIOC, 13); palClearPad(GPIOC, 14); palClearPad(GPIOC, 15)

// Output1
#define OUT_1_GPIO		    GPIOC
#define OUT_1_PIN			10
#define OUT_1_ON()		    palSetPad(OUT_1_GPIO, OUT_1_PIN)
#define OUT_1_OFF()		    palClearPad(OUT_1_GPIO, OUT_1_PIN)

// Output2
#define OUT_2_GPIO			GPIOC
#define OUT_2_PIN			11
#define OUT_2_ON()			palSetPad(OUT_2_GPIO, OUT_2_PIN)
#define OUT_2_OFF()		    palClearPad(OUT_2_GPIO, OUT_2_PIN)

// Output3
#define OUT_3_GPIO			GPIOA
#define OUT_3_PIN			15
#define OUT_3_ON()			palSetPad(OUT_3_GPIO, OUT_3_PIN)
#define OUT_3_OFF()		    palClearPad(OUT_3_GPIO, OUT_3_PIN)

// Shutdown pin
#define HW_SHUTDOWN_GPIO		GPIOA
#define HW_SHUTDOWN_PIN			5
#define HW_SHUTDOWN_HOLD_ON()	hw_shutdown_set_hold(true)
#define HW_SHUTDOWN_HOLD_OFF()	hw_shutdown_set_hold(false)
#define HW_SAMPLE_SHUTDOWN()	hw_sample_shutdown_button()
#define HW_SHUTDOWN_NO

// Hold shutdown pin early to wake up on short pulses
#define HW_EARLY_INIT()			HW_SHUTDOWN_HOLD_ON()

/*
 * ADC Vector
 *
 * 0  (1):	IN10	CURR1
 * 1  (2):	IN11	CURR2
 * 2  (3):	IN12	CURR3
 * 3  (1):	IN0		SENS1
 * 4  (2):	IN1		SENS2
 * 5  (3):	IN2		SENS3
 * 6  (1):	IN5
 * 7  (2):	IN6
 * 8  (3):	IN3
 * 9  (1):	IN14
 * 10 (2):	IN15
 * 11 (3):	IN13
 * 12 (1):	Vrefint
 * 13 (2):	IN7 Wheel speed
 * 14 (3):	IN1
 * 15 (1):  IN8
 * 16 (2):  IN9
 * 17 (3):  IN2
 */

#define HW_ADC_CHANNELS			18
#define HW_ADC_INJ_CHANNELS		3
#define HW_ADC_NBR_CONV			6

// ADC Indexes
#define ADC_IND_SENS1			3
#define ADC_IND_SENS2			4
#define ADC_IND_SENS3			5
#define ADC_IND_CURR1			0
#define ADC_IND_CURR2			1
#define ADC_IND_CURR3			2
#define ADC_IND_VIN_SENS		11
#define ADC_IND_EXT				8
#define ADC_IND_EXT2			15
#define ADC_IND_EXT3			13
#define ADC_IND_TEMP_MOS		10
#define ADC_IND_TEMP_MOTOR		16
#define ADC_IND_VREFINT			12
#define ADC_IND_SHUTDOWN		6

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					150000.0
#endif
#ifndef VIN_R2
#define VIN_R2					4700.0
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
#define NTC_RES(adc_val)		(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // MOS temp sensor on low side //((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind)		(1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4095.0 * V_REG)

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOA
#define HW_ADC_EXT_PIN			3
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
#define HW_USE_SERVO_TIM4
#define HW_ICU_TIMER			TIM4
#define HW_ICU_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ICU_DEV				ICUD4
#define HW_ICU_CHANNEL			ICU_CHANNEL_1
#define HW_ICU_GPIO_AF			GPIO_AF_TIM4
#define HW_ICU_GPIO				GPIOB
#define HW_ICU_PIN				6

// IMU
#define LSM6DS3_NSS_GPIO		GPIOC
#define LSM6DS3_NSS_PIN			4
#define LSM6DS3_SCK_GPIO		GPIOA
#define LSM6DS3_SCK_PIN			6
#define LSM6DS3_MOSI_GPIO		GPIOB
#define LSM6DS3_MOSI_PIN		3
#define LSM6DS3_MISO_GPIO		GPIOB
#define LSM6DS3_MISO_PIN		2

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

// SPI pins //Replicated from the IMU
#define HW_SPI_DEV				SPID1
#define HW_SPI_GPIO_AF			GPIO_AF_SPI1
#define HW_SPI_PORT_NSS			GPIOB
#define HW_SPI_PIN_NSS			11
#define HW_SPI_PORT_SCK			GPIOB
#define HW_SPI_PIN_SCK			2
#define HW_SPI_PORT_MOSI		GPIOB
#define HW_SPI_PIN_MOSI			2
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

#define HW_DEAD_TIME_NSEC		200.0

// Default setting overrides
#ifndef MCCONF_L_MIN_VOLTAGE
#define MCCONF_L_MIN_VOLTAGE			15.0		// Minimum voltage input
#endif
#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE			90.0	// Maximum input voltage
#endif
#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC
#endif
#ifndef MCCONF_FOC_F_ZV
#define MCCONF_FOC_F_ZV					30000.0
#endif
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT		80.0	// The maximum absolute current above which a fault is generated
#endif
#ifndef MCCONF_L_IN_CURRENT_MAX
#define MCCONF_L_IN_CURRENT_MAX			45.0	// Input current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_IN_CURRENT_MIN
#define MCCONF_L_IN_CURRENT_MIN			-45.0	// Input current limit in Amperes (Lower)
#endif

// Setting limits
#define HW_LIM_CURRENT			-65.0, 65.0
#define HW_LIM_CURRENT_IN		-60.0, 60.0
#define HW_LIM_CURRENT_ABS		0.0, 110.0
#define HW_LIM_VIN				11.0, 94.0
#define HW_LIM_ERPM				-200e3, 200e3
#define HW_LIM_DUTY_MIN			0.0, 0.1
#define HW_LIM_DUTY_MAX			0.0, 0.99
#define HW_LIM_TEMP_FET			-40.0, 110.0

bool hw_sample_shutdown_button(void);
void hw_shutdown_set_hold(bool hold);

#endif /* HW_MINIM_CORE_H_ */
