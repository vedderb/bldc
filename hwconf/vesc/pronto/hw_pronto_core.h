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

#ifndef HW_PRONTO_CORE_H_
#define HW_PRONTO_CORE_H_

#ifdef HWPRONTO
	#define HW_NAME					"Pronto"
#else
	#error "Must define hardware type"
#endif

// HW properties
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_FILTERS
#define HW_HAS_PHASE_SHUNTS
#define HW_BOOT_VESC_CAN

#define COMM_USE_USB				0

// Macros
#define LED_GREEN_GPIO			GPIOC
#define LED_GREEN_PIN			9
#define LED_RED_GPIO			GPIOC
#define LED_RED_PIN				12

#define LED_GREEN_ON()			palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF()			palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON()			palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF()			palClearPad(LED_RED_GPIO, LED_RED_PIN)

#define PHASE_FILTER_OFF()		palSetPad(GPIOB, 12); palSetPad(GPIOA, 11); palSetPad(GPIOA, 12)
#define PHASE_FILTER_ON()		palClearPad(GPIOB, 12); palClearPad(GPIOA, 11); palClearPad(GPIOA, 12)

#define CURRENT_FILTER_GPIO		GPIOC
#define CURRENT_FILTER_PIN		5
#define CURRENT_FILTER_ON()		palSetPad(CURRENT_FILTER_GPIO, CURRENT_FILTER_PIN)
#define CURRENT_FILTER_OFF()	palClearPad(CURRENT_FILTER_GPIO, CURRENT_FILTER_PIN)

// ADC Mux
#define ADC_SW_1_PORT			GPIOC
#define ADC_SW_1_PIN			14
#define ADC_SW_2_PORT			GPIOC
#define ADC_SW_2_PIN			15
#define ADC_SW_3_PORT			GPIOD
#define ADC_SW_3_PIN			2

#define AD1_L()					palClearPad(ADC_SW_1_PORT, ADC_SW_1_PIN)
#define AD1_H()					palSetPad(ADC_SW_1_PORT, ADC_SW_1_PIN)
#define AD2_L()					palClearPad(ADC_SW_2_PORT, ADC_SW_2_PIN)
#define AD2_H()					palSetPad(ADC_SW_2_PORT, ADC_SW_2_PIN)
#define AD3_L()					palClearPad(ADC_SW_3_PORT, ADC_SW_3_PIN)
#define AD3_H()					palSetPad(ADC_SW_3_PORT, ADC_SW_3_PIN)

#define ADCMUX_MOT_TEMP()		AD3_L();	AD2_L();	AD1_L();
#define ADCMUX_12V_SENSE_V()	AD3_L();	AD2_L();	AD1_H();
#define ADCMUX_MOS_TEMP()		AD3_L();	AD2_H();	AD1_L();
#define ADCMUX_12V_SENSE_I()	AD3_H();	AD2_L();	AD1_L();
#define ADCMUX_TEMP_DCDC()		AD3_H();	AD2_H();	AD1_H();

#define AUX_GPIO				GPIOB
#define AUX_PIN					7
#define AUX_ON()				palSetPad(AUX_GPIO, AUX_PIN)
#define AUX_OFF()				palClearPad(AUX_GPIO, AUX_PIN)

#define AUX2_GPIO				GPIOC
#define AUX2_PIN				13
#define AUX2_ON()				palSetPad(AUX2_GPIO, AUX2_PIN)
#define AUX2_OFF()				palClearPad(AUX2_GPIO, AUX2_PIN)

// Shutdown pin
#define HW_SHUTDOWN_GPIO		GPIOB
#define HW_SHUTDOWN_PIN			2
#define HW_SHUTDOWN_HOLD_ON()	palSetPad(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN)
#define HW_SHUTDOWN_HOLD_OFF()	palClearPad(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN)
#define HW_SAMPLE_SHUTDOWN()	hw_sample_shutdown_button()
#define HW_SHUTDOWN_NO // Normally open button

// Hold shutdown pin early to wake up on short pulses
#define HW_EARLY_INIT()			palSetPadMode(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN, PAL_MODE_OUTPUT_PUSHPULL); \
								HW_SHUTDOWN_HOLD_ON();

#define MCPWM_FOC_CURRENT_SAMP_OFFSET				(2) // Offset from timer top for ADC samples

/*
 * ADC Vector
 */
#define HW_ADC_CHANNELS			18
#define HW_ADC_CHANNELS_EXTRA	8
#define HW_ADC_INJ_CHANNELS		3
#define HW_ADC_NBR_CONV			6

// ADC Indicies
#define ADC_IND_CURR1			0
#define ADC_IND_CURR2			1
#define ADC_IND_CURR3			2
#define ADC_IND_SENS1			3
#define ADC_IND_SENS2			4
#define ADC_IND_SENS3			5
#define ADC_IND_VIN_SENS		8
#define ADC_IND_EXT5			16
#define ADC_IND_EXT				6
#define ADC_IND_EXT2			7
#define ADC_IND_EXT3			14
#define ADC_IND_ADC_MUX			15
#define ADC_IND_EXT4			12
#define ADC_IND_SHUTDOWN		13
#define ADC_IND_EXT6			13

#define ADC_IND_TEMP_MOTOR		18
#define ADC_IND_12V_SENSE_V		19
#define ADC_IND_TEMP_MOS		20
#define ADC_IND_12V_SENSE_I		22
#define ADC_IND_TEMP_DCDC		25

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
#define CURRENT_SHUNT_RES		0.00025
#endif

#define ENCODER_SIN_VOLTS		ADC_VOLTS(ADC_IND_EXT4)
#define ENCODER_COS_VOLTS		ADC_VOLTS(ADC_IND_EXT3)

// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
#define NTC_RES(adc_val)		(10000.0 / ((4095.0 / (float)adc_val) - 1.0))
#define NTC_TEMP(adc_ind)		(1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

#define NTC_TEMP_DCDC()			(1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_DCDC]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4096.0 * V_REG)

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOA
#define HW_ADC_EXT_PIN			7
#define HW_ADC_EXT2_GPIO		GPIOA
#define HW_ADC_EXT2_PIN			6
#define HW_ADC_EXT3_GPIO		GPIOA
#define HW_ADC_EXT3_PIN			3
#define HW_ADC_EXT4_GPIO		GPIOC
#define HW_ADC_EXT4_PIN			4

// UART Peripheral
#define HW_UART_DEV				SD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOB
#define HW_UART_TX_PIN			10
#define HW_UART_RX_PORT			GPIOB
#define HW_UART_RX_PIN			11

// Permanent UART Peripheral (SWD/ESP)
#define HW_UART_P_BAUD			115200
#define HW_UART_P_DEV			SD4
#define HW_UART_P_GPIO_AF		GPIO_AF_UART4
#define HW_UART_P_TX_PORT		GPIOC
#define HW_UART_P_TX_PIN		10
#define HW_UART_P_RX_PORT		GPIOC
#define HW_UART_P_RX_PIN		11

// ICU Peripheral for servo decoding
#define HW_USE_SERVO_TIM4
#define HW_ICU_TIMER			TIM4
#define HW_ICU_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
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
#define HW_SPI_DEV				SPID1
#define HW_SPI_GPIO_AF			GPIO_AF_SPI1
#define HW_SPI_PORT_NSS			GPIOB
#define HW_SPI_PIN_NSS			11
//#define HW_SPI_PORT_SCK			GPIOA
//#define HW_SPI_PIN_SCK			5
#define HW_SPI_PORT_MOSI		GPIOA
#define HW_SPI_PIN_MOSI			7
#define HW_SPI_PORT_MISO		GPIOA
#define HW_SPI_PIN_MISO			6

// IMU
#define LSM6DS3_NSS_GPIO		GPIOA
#define LSM6DS3_NSS_PIN			15
#define LSM6DS3_SCK_GPIO		GPIOB
#define LSM6DS3_SCK_PIN			3
#define LSM6DS3_MOSI_GPIO		GPIOB
#define LSM6DS3_MOSI_PIN		5
#define LSM6DS3_MISO_GPIO		GPIOB
#define LSM6DS3_MISO_PIN		4

// Measurement macros
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO				(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

#define HW_DEAD_TIME_NSEC		300.0

// Default setting overrides
#ifndef MCCONF_L_MIN_VOLTAGE
#define MCCONF_L_MIN_VOLTAGE			20.0		// Minimum input voltage
#endif
#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE			94.0	// Maximum input voltage
#endif
#ifndef MCCONF_FOC_F_ZV
#define MCCONF_FOC_F_ZV					30000.0
#endif
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT		120.0	// The maximum absolute current above which a fault is generated
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
#ifndef APPCONF_SHUTDOWN_MODE
#define APPCONF_SHUTDOWN_MODE			SHUTDOWN_MODE_ALWAYS_ON
#endif
#ifndef APPCONF_APP_TO_USE
#define APPCONF_APP_TO_USE				APP_NONE
#endif

// Setting limits
#define HW_LIM_CURRENT			-200.0, 200.0
#define HW_LIM_CURRENT_IN		-200.0, 200.0
#define HW_LIM_CURRENT_ABS		0.0, 300.0
#define HW_LIM_VIN				20.0, 97.0
#define HW_LIM_ERPM				-200e3, 200e3
#define HW_LIM_DUTY_MIN			0.0, 0.1
#define HW_LIM_DUTY_MAX			0.0, 0.99
#define HW_LIM_TEMP_FET			-40.0, 110.0

// HW-specific functions
bool hw_sample_shutdown_button(void);

#endif /* HW_PRONTO_CORE_H_ */
