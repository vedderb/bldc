/*
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

#ifndef HW_IVY_100V_HP_H_
#define HW_IVY_100V_HP_H_

#define HW_NAME "IVY_100V_HP"

// Default setting overrides
#define APPCONF_APP_TO_USE APP_NONE
// #define APPCONF_CONTROLLER_ID 2
#define APPCONF_TIMEOUT_MSEC 500
#define APPCONF_TIMEOUT_BRAKE_CURRENT 0
#define APPCONF_PERMANENT_UART_ENABLED false
#define APPCONF_SHUTDOWN_MODE SHUTDOWN_MODE_ALWAYS_ON
#define APPCONF_CAN_BAUD_RATE CAN_BAUD_1M
#define APPCONF_CAN_STATUS_RATE_1 100
#define APPCONF_CAN_STATUS_RATE_2 5
#define APPCONF_CAN_STATUS_MSGS_R1 0x09
#define APPCONF_CAN_STATUS_MSGS_R2 0x10
// Common PID-parameters
#define MCCONF_SP_PID_LOOP_RATE PID_RATE_5000_HZ // PID loop rate
// Speed PID parameters
#define MCCONF_S_PID_KP 0.003            // Proportional gain
#define MCCONF_S_PID_KI 0.009            // Integral gain
#define MCCONF_S_PID_KD 0.00002          // Derivative gain
#define MCCONF_S_PID_KD_FILTER 0.2       // Derivative filter
#define MCCONF_S_PID_MIN_RPM 300.0       // Minimum allowed RPM
#define MCCONF_S_PID_ALLOW_BRAKING true  // Allow braking in speed control mode
#define MCCONF_S_PID_RAMP_ERPMS_S 2000.0 // Speed input ramping, in ERPM/s
#define MCCONF_S_PID_SPEED_SOURCE S_PID_SPEED_SRC_PLL
// Position PID parameters
#define MCCONF_P_PID_KP 0.03              // Proportional gain
#define MCCONF_P_PID_KI 0.0               // Integral gain
#define MCCONF_P_PID_KD 0.00000           // Derivative gain
#define MCCONF_P_PID_KD_PROC 0.00035      // Derivative gain process
#define MCCONF_P_PID_KD_FILTER 0.2        // Derivative filter
#define MCCONF_P_PID_ANG_DIV 10.0         // Divide angle by this value
#define MCCONF_P_PID_GAIN_DEC_ANGLE 500.0 // Decrease PID-gains when the error is below this value
#define MCCONF_P_PID_OFFSET 0.0           // Angle offset

#define MCCONF_L_MIN_VOLTAGE 20.0 // Minimum input voltage
#define MCCONF_L_MAX_VOLTAGE 95.0 // Maximum input voltage
#define MCCONF_DEFAULT_MOTOR_TYPE MOTOR_TYPE_FOC

#define MCCONF_L_MAX_ABS_CURRENT 320.0 // The maximum absolute current above which a fault is generated
#define MCCONF_L_IN_CURRENT_MAX 200.0  // Input current limit in Amperes (Upper)
#define MCCONF_L_IN_CURRENT_MIN -200.0 // Input current limit in Amperes (Lower)

#define MCCONF_FOC_F_ZV 15000.0
#define MCCONF_FOC_CONTROL_SAMPLE_MODE FOC_CONTROL_SAMPLE_MODE_V0_V7
#define MCCONF_FOC_SAMPLE_V0_V7 true // Run control loop in both v0 and v7 (requires phase shunts)

// Override dead time. See the stm32f4 reference manual for calculating this value.
#define HW_DEAD_TIME_NSEC 500.0

// HW properties
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_SHUNTS
#define HW_HAS_PHASE_FILTERS
#define HW_HAS_CURR_FILTERS

// Macros
#define LED_GREEN_GPIO GPIOB
#define LED_GREEN_PIN 5
#define LED_RED_GPIO GPIOB
#define LED_RED_PIN 7

#define LED_GREEN_ON() palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF() palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON() palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF() palClearPad(LED_RED_GPIO, LED_RED_PIN)

#ifdef HW_HAS_PHASE_FILTERS
#define PHASE_FILTER_GPIO GPIOC
#define PHASE_FILTER_PIN 9
#define PHASE_FILTER_ON() palSetPad(PHASE_FILTER_GPIO, PHASE_FILTER_PIN)
#define PHASE_FILTER_OFF() palClearPad(PHASE_FILTER_GPIO, PHASE_FILTER_PIN)
#endif

// Shutdown pin
#define HW_SHUTDOWN_GPIO GPIOC
#define HW_SHUTDOWN_PIN 5
#define HW_SHUTDOWN_HOLD_ON() palSetPad(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN)
#define HW_SHUTDOWN_HOLD_OFF() palClearPad(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN)
#define HW_SAMPLE_SHUTDOWN() hw_sample_shutdown_button()

// Hold shutdown pin early to wake up on short pulses
#define HW_EARLY_INIT()                                                         \
    palSetPadMode(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN, PAL_MODE_OUTPUT_PUSHPULL); \
    HW_SHUTDOWN_HOLD_ON();

#ifdef HW_HAS_CURR_FILTERS
#define CURRENT_FILTER_ON() palSetPad(GPIOD, 2)
#define CURRENT_FILTER_OFF() palClearPad(GPIOD, 2)
#endif

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
 * 10 (2):	IN15	SHUTDOWN
 * 11 (3):	IN13	AN_IN
 * 12 (1):	Vrefint
 * 13 (2):	IN0		SENS1
 * 14 (3):	IN1		SENS2
 * 15 (1):  IN8		TEMP_MOS_2
 * 16 (2):  IN9		TEMP_MOS_3
 * 17 (3):  IN3		SENS3
 */

#define HW_ADC_CHANNELS 18
#define HW_ADC_INJ_CHANNELS 3
#define HW_ADC_NBR_CONV 6

// ADC Indexes
#define ADC_IND_SENS1 0
#define ADC_IND_SENS2 1
#define ADC_IND_SENS3 2
#define ADC_IND_CURR1 3
#define ADC_IND_CURR2 4
#define ADC_IND_CURR3 5
#define ADC_IND_VIN_SENS 11
#define ADC_IND_EXT 6
#define ADC_IND_EXT2 7
#define ADC_IND_SHUTDOWN 10
#define ADC_IND_TEMP_MOS 8
#define ADC_IND_TEMP_MOTOR 9
#define ADC_IND_VREFINT 12

// ADC macros and settings

// Component parameters
#define V_REG 3.30

// The voltage dividing acquisition circuit on the Makerbase VESC motherboard is 560K and 21.5K resistors.
#define VIN_R1 750000.0
#define VIN_R2 22000.0

#define CURRENT_AMP_GAIN 20.0
#define CURRENT_SHUNT_RES (0.0005 / 2.0)

// Input voltage
#define GET_INPUT_VOLTAGE() ((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
// #define NTC_RES(adc_val) ((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_RES(adc_val) ((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind) (1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3435.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val) (10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta) (1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch) ((float)ADC_Value[ch] / 4096.0 * V_REG)

// Double samples in beginning and end for positive current measurement.
// Useful when the shunt sense traces have noise that causes offset.
#define CURR1_DOUBLE_SAMPLE 0
#define CURR2_DOUBLE_SAMPLE 0
#define CURR3_DOUBLE_SAMPLE 0

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO GPIOA
#define HW_ADC_EXT_PIN 5
#define HW_ADC_EXT2_GPIO GPIOA
#define HW_ADC_EXT2_PIN 6

// UART Peripheral
#define HW_UART_DEV SD3
#define HW_UART_GPIO_AF GPIO_AF_USART3
#define HW_UART_TX_PORT GPIOB
#define HW_UART_TX_PIN 10
#define HW_UART_RX_PORT GPIOB
#define HW_UART_RX_PIN 11

// Permanent UART Peripheral (for NRF51)
#define HW_UART_P_BAUD 115200
#define HW_UART_P_DEV SD4
#define HW_UART_P_GPIO_AF GPIO_AF_UART4
#define HW_UART_P_TX_PORT GPIOC
#define HW_UART_P_TX_PIN 10
#define HW_UART_P_RX_PORT GPIOC
#define HW_UART_P_RX_PIN 11

// ICU Peripheral for servo decoding
#define HW_USE_SERVO_TIM4
#define HW_ICU_TIMER TIM4
#define HW_ICU_TIM_CLK_EN() RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ICU_DEV ICUD4
#define HW_ICU_CHANNEL ICU_CHANNEL_1
#define HW_ICU_GPIO_AF GPIO_AF_TIM4
#define HW_ICU_GPIO GPIOB
#define HW_ICU_PIN 6

// I2C Peripheral
#define HW_I2C_DEV I2CD2
#define HW_I2C_GPIO_AF GPIO_AF_I2C2
#define HW_I2C_SCL_PORT GPIOB
#define HW_I2C_SCL_PIN 10
#define HW_I2C_SDA_PORT GPIOB
#define HW_I2C_SDA_PIN 11

// Hall/encoder pins
#define HW_HALL_ENC_GPIO1 GPIOC
#define HW_HALL_ENC_PIN1 6
#define HW_HALL_ENC_GPIO2 GPIOC
#define HW_HALL_ENC_PIN2 7
#define HW_HALL_ENC_GPIO3 GPIOC
#define HW_HALL_ENC_PIN3 8
#define HW_ENC_TIM TIM3
#define HW_ENC_TIM_AF GPIO_AF_TIM3
#define HW_ENC_TIM_CLK_EN() RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE)
#define HW_ENC_EXTI_PORTSRC EXTI_PortSourceGPIOC
#define HW_ENC_EXTI_PINSRC EXTI_PinSource8
#define HW_ENC_EXTI_CH EXTI9_5_IRQn
#define HW_ENC_EXTI_LINE EXTI_Line8
#define HW_ENC_EXTI_ISR_VEC EXTI9_5_IRQHandler
#define HW_ENC_TIM_ISR_CH TIM3_IRQn
#define HW_ENC_TIM_ISR_VEC TIM3_IRQHandler

// SPI pins
#define HW_SPI_DEV SPID1
#define HW_SPI_GPIO_AF GPIO_AF_SPI1
#define HW_SPI_PORT_NSS GPIOA
#define HW_SPI_PIN_NSS 4
#define HW_SPI_PORT_SCK GPIOA
#define HW_SPI_PIN_SCK 5
#define HW_SPI_PORT_MOSI GPIOA
#define HW_SPI_PIN_MOSI 7
#define HW_SPI_PORT_MISO GPIOA
#define HW_SPI_PIN_MISO 6

// Measurement macros
#define ADC_V_L1 ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2 ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3 ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO (ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1() palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2() palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3() palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Setting limits
#define HW_LIM_CURRENT -300.0, 300.0
#define HW_LIM_CURRENT_IN -280.0, 280.0
#define HW_LIM_CURRENT_ABS 0.0, 320
#define HW_LIM_VIN 12.0, 90.0
#define HW_LIM_ERPM -200e3, 200e3
#define HW_LIM_DUTY_MIN 0.0, 0.1
#define HW_LIM_DUTY_MAX 0.0, 0.99
#define HW_LIM_TEMP_FET -40.0, 110.0

// HW-specific functions
bool hw_sample_shutdown_button(void);
#endif /* HW_IVY_100V_HP_H_ */
