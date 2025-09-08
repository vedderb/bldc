/*
    Copyright 2019 Benjamin Vedder  benjamin@vedder.se

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

#ifndef HW_EPC9146_H_
#define HW_EPC9146_H_

#define HW_NAME							"IPM_EPC9146"

// HW properties
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_SHUNTS
#define HW_HAS_PHASE_FILTERS
// #define	HW_AXIOM_FORCE_HIGH_CURRENT_MEASUREMENTS
// #define	HW_HIGH_F_MEASUREMENTS
#define INVERTED_SHUNT_POLARITY

// Macros
#define LED_GREEN_GPIO			GPIOB
#define LED_GREEN_PIN			0
#define LED_RED_GPIO			GPIOB
#define LED_RED_PIN				1

#define LED_GREEN_ON()			palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF()			palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON()			palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF()			palClearPad(LED_RED_GPIO, LED_RED_PIN)

#define IS_DRV_FAULT()			(!palReadPad(GPIOB, 7))

#define LED_GREEN_ON()			palSetPad(GPIOB, 0)
#define LED_GREEN_OFF()			palClearPad(GPIOB, 0)
#define LED_RED_ON()			palSetPad(GPIOB, 1)
#define LED_RED_OFF()			palClearPad(GPIOB, 1)

// #define PHASE_FILTER_GPIO		GPIOC
// #define PHASE_FILTER_PIN		11

// #define PHASE_FILTER_ON()		palSetPad(PHASE_FILTER_GPIO, PHASE_FILTER_PIN)
// #define PHASE_FILTER_OFF()		palClearPad(PHASE_FILTER_GPIO, PHASE_FILTER_PIN)

// #define AUX_GPIO				GPIOC
// #define AUX_PIN					12
// #define AUX_ON()				palSetPad(AUX_GPIO, AUX_PIN)
// #define AUX_OFF()				palClearPad(AUX_GPIO, AUX_PIN)

// #define CURRENT_FILTER_ON()		palSetPad(GPIOD, 2)
// #define CURRENT_FILTER_OFF()	palClearPad(GPIOD, 2)

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
 * 10 (2):	IN15	ADC_EXT3
 * 11 (3):	IN13	AN_IN
 * 12 (1):	Vrefint
 * 13 (2):	IN0		SENS1
 * 14 (3):	IN1		SENS2
 * 15 (1):  IN8		TEMP_MOS_2
 * 16 (2):  IN9		TEMP_MOS_3
 * 17 (3):  IN3		SENS3
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
#define ADC_IND_EXT2			7
#define ADC_IND_TEMP_MOS		8
#define ADC_IND_TEMP_MOTOR		9
#define ADC_IND_VREFINT			12

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					100000.0
#endif
#ifndef VIN_R2
#define VIN_R2					4220.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN		50.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES		0.0015
#endif

// Input voltage
#define GET_INPUT_VOLTAGE()     ((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
#define NTC_RES(adc_val)        (10000.0 / ((4095.0 / (float)adc_val) - 1.0)) //NTC is low side onb 

//AD590
// #define R_tem	  7870.0
#define R_tem	  6340.0
#define Kelvin_A  0.000001
#define Kelvin_V  (float)(R_tem*Kelvin_A)
#define NTC_TEMP(adc_ind)       ((((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_TEMP_MOS])/Kelvin_V) - 273.15)

#define NTC_RES_MOTOR(adc_val)  (10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)    (1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch)           ((float)ADC_Value[ch] / 4096.0 * V_REG)

// Double samples in beginning and end for positive current measurement.
// Useful when the shunt sense traces have noise that causes offset.
#ifndef CURR1_DOUBLE_SAMPLE
#define CURR1_DOUBLE_SAMPLE     0
#endif
#ifndef CURR2_DOUBLE_SAMPLE
#define CURR2_DOUBLE_SAMPLE     0
#endif
#ifndef CURR3_DOUBLE_SAMPLE
#define CURR3_DOUBLE_SAMPLE     0
#endif

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOA
#define HW_ADC_EXT_PIN			5
#define HW_ADC_EXT2_GPIO		GPIOA
#define HW_ADC_EXT2_PIN			6

// Number of servo outputs
#define HW_SERVO_NUM            1

// UART Peripheral
#define HW_UART_DEV             SD3
#define HW_UART_GPIO_AF         GPIO_AF_USART3
#define HW_UART_TX_PORT         GPIOB
#define HW_UART_TX_PIN          10
#define HW_UART_RX_PORT         GPIOB
#define HW_UART_RX_PIN          11


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
#define HW_I2C_DEV              I2CD2
#define HW_I2C_GPIO_AF          GPIO_AF_I2C2
#define HW_I2C_SCL_PORT         GPIOB
#define HW_I2C_SCL_PIN          10
#define HW_I2C_SDA_PORT         GPIOB
#define HW_I2C_SDA_PIN          11

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
#define HW_SPI_DEV              SPID1
#define HW_SPI_GPIO_AF          GPIO_AF_SPI1
#define HW_SPI_PORT_NSS         GPIOA
#define HW_SPI_PIN_NSS          4
#define HW_SPI_PORT_SCK         GPIOA
#define HW_SPI_PIN_SCK          5
#define HW_SPI_PORT_MOSI        GPIOA
#define HW_SPI_PIN_MOSI         7
#define HW_SPI_PORT_MISO        GPIOA
#define HW_SPI_PIN_MISO         6

// Measurement macros
#define ADC_V_L1                ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2                ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3                ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO              (ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()            palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()            palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()            palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Default setting overrides
#ifndef MCCONF_L_MIN_VOLTAGE
#define MCCONF_L_MIN_VOLTAGE			    14.0		// Minimum input voltage
#endif
#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE                81.5    // Maximum input voltage
#endif
#ifndef MCCONF_L_CURRENT_MAX
#define MCCONF_L_CURRENT_MAX                21.3    // Current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_CURRENT_MIN
#define MCCONF_L_CURRENT_MIN                -21.3   // Current limit in Amperes (Lower)
#endif
#ifndef MCCONF_L_IN_CURRENT_MAX
#define MCCONF_L_IN_CURRENT_MAX             21.3    // Input current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_IN_CURRENT_MIN
#define MCCONF_L_IN_CURRENT_MIN             -21.3   // Input current limit in Amperes (Lower)
#endif
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT            42.6   // The maximum absolute current above which a fault is generated
#endif
#ifndef MCCONF_FOC_SAMPLE_V0_V7
#define MCCONF_FOC_SAMPLE_V0_V7			false	// Run control loop in both v0 and v7 (requires phase shunts)
#endif

#ifndef MCCONF_FOC_DT_US
#define MCCONF_FOC_DT_US			0 // Microseconds for dead time compensation
#endif

#define HW_DEAD_TIME_NSEC		100.0

#define MCCONF_DEFAULT_MOTOR_TYPE       MOTOR_TYPE_FOC
#ifndef MCCONF_FOC_F_ZV
#define MCCONF_FOC_F_ZV                 50000.0
#endif

// Openloop ERPM
#define MCCONF_FOC_OPENLOOP_RPM 900

// Openloop ERPM at Min Current
// #define MCCONF_FOC_OPENLOOP_RPM_LOW 0

// D Axis Gain Scaling Start
// #define MCCONF_FOC_D_GAIN_SCALE_START 1

// D Axis Gain Scaling at Max Mod
// #define MCCONF_FOC_D_GAIN_SCALE_MAX_MOD 0.2

// Openloop Hysteresis
#define MCCONF_FOC_SL_OPENLOOP_HYST 0.02

// Openloop Lock Time
#define MCCONF_FOC_SL_OPENLOOP_T_LOCK 0

// Openloop Ramp Time
#define MCCONF_FOC_SL_OPENLOOP_T_RAMP 0.01

// Openloop Time
#define MCCONF_FOC_SL_OPENLOOP_TIME 0.01

// Decrease current to this fraction at start
#define MCCONF_FOC_START_CURR_DEC		0.3	

#define FOC_CONTROL_LOOP_FREQ_DIVIDER	1

// Setting limits
#define HW_LIM_CURRENT          -21.3, 21.3 // -21.66, 21.66 
#define HW_LIM_CURRENT_IN       -21.3, 21.3
#define HW_LIM_CURRENT_ABS      0.0, 42.6
#define HW_LIM_VIN              14.0, 81.5
#define HW_LIM_ERPM             -200e3, 200e3
#define HW_LIM_DUTY_MIN         0.0, 0.1
#define HW_LIM_DUTY_MAX         0.0, 0.99
#define HW_LIM_TEMP_FET         -55.0, 150.0

#endif /* HW_EPC9146_H_ */
