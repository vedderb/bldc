/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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
#ifndef HW_UNITY_H_
#define HW_UNITY_H_

#ifndef HW_HAS_DUAL_PARALLEL
#define HW_HAS_DUAL_MOTORS
#endif

#ifdef HW_HAS_DUAL_PARALLEL
#define HW_NAME                 "UNITY_PARALLEL"
#else
#define HW_NAME                 "UNITY"
#endif

#define HW_HAS_DRV8301
//#define HW_HAS_3_SHUNTS
//#define HW_HAS_PHASE_SHUNTS

// Switch Pins
#define SWITCH_IN_GPIO							GPIOE
#define SWITCH_IN_PIN							15
#define SWITCH_OUT_GPIO							GPIOB
#define SWITCH_OUT_PIN							13
#define SWITCH_LED_1_GPIO						GPIOD
#define SWITCH_LED_1_PIN						10
#define SWITCH_LED_2_GPIO						GPIOD
#define SWITCH_LED_2_PIN						11

#define HW_SHUTDOWN_HOLD_ON();
#define HW_SAMPLE_SHUTDOWN()   1
#define HW_SHUTDOWN_HOLD_OFF()	palClearPad(SWITCH_OUT_GPIO, SWITCH_OUT_PIN);

#define LED_PWM1_ON()							palClearPad(SWITCH_LED_1_GPIO,SWITCH_LED_1_PIN)
#define LED_PWM1_OFF()							palSetPad(SWITCH_LED_1_GPIO,SWITCH_LED_1_PIN)
#define LED_PWM2_ON()							palClearPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)
#define LED_PWM2_OFF()							palSetPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)

#define SMART_SWITCH_MSECS_PRESSED_OFF			2000

#define HW_EARLY_INIT()							smart_switch_pin_init(); LED_PWM1_ON(); smart_switch_thread_start();

// Pins for BLE UART
#define HW_UART_P_BAUD							250000
#define HW_UART_P_DEV							SD1
#define HW_UART_P_GPIO_AF						GPIO_AF_USART1
#define HW_UART_P_TX_PORT						GPIOA
#define HW_UART_P_TX_PIN						9
#define HW_UART_P_RX_PORT						GPIOA
#define HW_UART_P_RX_PIN						10

// Pins for Third UART - on black connector next to bluetooth module
#define HW_UART_3_BAUD                          115200
#define HW_UART_3_DEV                           SD2
#define HW_UART_3_GPIO_AF                       GPIO_AF_USART2
#define HW_UART_3_TX_PORT                       GPIOD
#define HW_UART_3_TX_PIN                        6
#define HW_UART_3_RX_PORT                       GPIOD
#define HW_UART_3_RX_PIN                        5

// SPI for DRV8301
#define HW_HAS_DRV8301
#define DRV8301_MOSI_GPIO						GPIOC
#define DRV8301_MOSI_PIN						12
#define DRV8301_MISO_GPIO						GPIOC
#define DRV8301_MISO_PIN						11
#define DRV8301_SCK_GPIO						GPIOC
#define DRV8301_SCK_PIN							10
#define DRV8301_CS_GPIO							GPIOC
#define DRV8301_CS_PIN							13
#define DRV8301_CS_GPIO2						GPIOE
#define DRV8301_CS_PIN2							0

// Macros
#define ENABLE_GATE()							palSetPad(GPIOE, 2); palSetPad(GPIOD, 4)
#define DISABLE_GATE()							palClearPad(GPIOE, 2); palClearPad(GPIOD, 4)
//#define ENABLE_GATE2()							palSetPad(GPIOD, 4)
//#define DISABLE_GATE2()							palClearPad(GPIOD, 4)

#define ENABLE_MOS_TEMP1()						palSetPad(GPIOE, 7); palClearPad(GPIOD, 8);\
		palClearPad(GPIOD, 9); palClearPad(GPIOB, 12);
#define ENABLE_MOS_TEMP2()						palSetPad(GPIOD, 8); palClearPad(GPIOE, 7);\
		palClearPad(GPIOD, 9); palClearPad(GPIOB, 12);
#define ENABLE_MOT_TEMP1()						palSetPad(GPIOD, 9); palClearPad(GPIOE, 7);\
		palClearPad(GPIOD, 8); palClearPad(GPIOB, 12);
#define ENABLE_MOT_TEMP2()						palSetPad(GPIOB, 12); palClearPad(GPIOE, 7);\
		palClearPad(GPIOD, 8); palClearPad(GPIOD, 9);

#define DCCAL_ON()								palSetPad(GPIOB, 8)
#define DCCAL_OFF()								palClearPad(GPIOB, 8)

#define IS_DRV_FAULT()							(!palReadPad(GPIOE, 3))
#define IS_DRV_FAULT_2()						(!palReadPad(GPIOE, 1))

#define LED_GREEN_ON()							palSetPad(GPIOD, 7)
#define LED_GREEN_OFF()							palClearPad(GPIOD, 7)
#define LED_RED_ON()							palSetPad(GPIOD, 3)
#define LED_RED_OFF()							palClearPad(GPIOD, 3)

/*
 * ADC Vector
 *
 * 0:  IN14    CURR3
 * 1:  IN15    CURR4
 * 2:  IN3     SERVO2/ADC
 * 3:   IN9     CURR1
 * 4:   IN8     CURR2
 * 5:   IN10    AN_IN
 * 6:   IN0     SENS2
 * 7:   IN1     SENS3
 * 8:   IN2     SENS1
 * 9:   IN5     ADC_EXT
 * 10:   IN4     ADC_TEMP
 * 11:   IN13    SENS4
 * 12:   Vrefint
 * 13:   IN11    SENS6
 * 14:  IN12    SENS5
 * 15:  IN6     ADC_EXT2
 */

#define HW_ADC_CHANNELS			15
#define HW_ADC_CHANNELS_EXTRA	4
#define HW_ADC_INJ_CHANNELS		2
#define HW_ADC_NBR_CONV			5

// ADC Indexes
#define ADC_IND_SENS1			8
#define ADC_IND_SENS2			6
#define ADC_IND_SENS3			7
#define ADC_IND_SENS4			11
#define ADC_IND_SENS5			14
#define ADC_IND_SENS6			13
#define ADC_IND_CURR1			3
#define ADC_IND_CURR2			4
#define ADC_IND_CURR4			0
#define ADC_IND_CURR5			1
#define ADC_IND_VIN_SENS		5
#define ADC_IND_EXT				9
#define ADC_IND_EXT2			15 // ??
#define ADC_IND_VREFINT			12
#define ADC_IND_ADC_MUX			10
#define ADC_IND_TEMP_MOS		15
#define ADC_IND_TEMP_MOS_M2		16
#define ADC_IND_TEMP_MOTOR		17
#define ADC_IND_TEMP_MOTOR_2	18

#define ADC_IND_CURR3			4
#define ADC_IND_CURR6			4

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG 					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					39000.0
#endif
#ifndef VIN_R2
#define VIN_R2					2200.0
#endif

#define INVERTED_SHUNT_POLARITY
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN		10.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES		0.001
#endif

// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4095.0 * V_REG)

// NTC Termistors
#define NTC_RES(adc_val)		(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side // High side ->((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind)		(1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3434.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)
#define NTC_TEMP_MOTOR_2(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR_2]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// UART Peripheral
#define HW_UART_DEV				SD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOB
#define HW_UART_TX_PIN			10
#define HW_UART_RX_PORT			GPIOB
#define HW_UART_RX_PIN			11

// ICU Peripheral for servo decoding
#define HW_ICU_TIMER			TIM9
#define HW_ICU_TIM_CLK_EN()		RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM9, ENABLE)
#define HW_ICU_DEV				ICUD9
#define HW_ICU_CHANNEL			ICU_CHANNEL_1
#define HW_ICU_GPIO_AF			GPIO_AF_TIM9
#define HW_ICU_GPIO				GPIOE
#define HW_ICU_PIN				5

// I2C Peripheral
#define HW_I2C_DEV				I2CD2
#define HW_I2C_GPIO_AF			GPIO_AF_I2C2
#define HW_I2C_SCL_PORT			GPIOB
#define HW_I2C_SCL_PIN			10
#define HW_I2C_SDA_PORT			GPIOB
#define HW_I2C_SDA_PIN			11

// Hall/encoder pins
#define HW_HALL_ENC_GPIO1		GPIOD
#define HW_HALL_ENC_PIN1		13
#define HW_HALL_ENC_GPIO2		GPIOD
#define HW_HALL_ENC_PIN2		12
#define HW_HALL_ENC_GPIO3		GPIOD
#define HW_HALL_ENC_PIN3		14
#define HW_ENC_TIM				TIM4
#define HW_ENC_TIM_AF			GPIO_AF_TIM4
#define HW_ENC_TIM_CLK_EN()		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ENC_EXTI_PORTSRC		EXTI_PortSourceGPIOD
#define HW_ENC_EXTI_PINSRC		EXTI_PinSource14
#define HW_ENC_EXTI_CH			EXTI15_10_IRQn
#define HW_ENC_EXTI_LINE		EXTI_Line14
#define HW_ENC_EXTI_ISR_VEC		EXTI15_10_IRQHandler
#define HW_ENC_TIM_ISR_CH		TIM4_IRQn
#define HW_ENC_TIM_ISR_VEC		TIM4_IRQHandler

// Second hall/encoder. TODO: Implement encoder support for second motor.
#define HW_HALL_ENC_GPIO4		GPIOB
#define HW_HALL_ENC_PIN4		4
#define HW_HALL_ENC_GPIO5		GPIOB
#define HW_HALL_ENC_PIN5		6
#define HW_HALL_ENC_GPIO6		GPIOB
#define HW_HALL_ENC_PIN6		7
#define HW_ENC_TIM2				TIM3
#define HW_ENC_TIM_AF2			GPIO_AF_TIM3
#define HW_ENC_TIM_CLK_EN2()	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE)
#define HW_ENC_EXTI_PORTSRC2	EXTI_PortSourceGPIOB
#define HW_ENC_EXTI_PINSRC2		EXTI_PinSource7
#define HW_ENC_EXTI_CH2			EXTI9_5_IRQn
#define HW_ENC_EXTI_LINE2		EXTI_Line6
#define HW_ENC_EXTI_ISR_VEC2	EXTI9_5_IRQHandler
#define HW_ENC_TIM_ISR_CH2		TIM3_IRQn
#define HW_ENC_TIM_ISR_VEC2		TIM3_IRQHandler

// NRF pins
#define NRF_PORT_CSN			GPIOD
#define NRF_PIN_CSN				3
#define NRF_PORT_SCK			GPIOD
#define NRF_PIN_SCK				2
#define NRF_PORT_MOSI			GPIOD
#define NRF_PIN_MOSI			11
#define NRF_PORT_MISO			GPIOD
#define NRF_PIN_MISO			10

#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE	MOTOR_TYPE_FOC
#endif

// SPI pins
#define HW_SPI_DEV				SPID1
#define HW_SPI_GPIO_AF			GPIO_AF_SPI1
#define HW_SPI_PORT_NSS			GPIOB
#define HW_SPI_PIN_NSS			11
#define HW_SPI_PORT_SCK			GPIOA
#define HW_SPI_PIN_SCK			5
#define HW_SPI_PORT_MOSI		GPIOB
#define HW_SPI_PIN_MOSI			5
#define HW_SPI_PORT_MISO		GPIOA
#define HW_SPI_PIN_MISO			6

// LSM6DS3
#define LSM6DS3_SDA_GPIO		GPIOC
#define LSM6DS3_SDA_PIN			9
#define LSM6DS3_SCL_GPIO		GPIOA
#define LSM6DS3_SCL_PIN			8

// CAN-bus
#define HW_CANRX_PORT			GPIOD
#define HW_CANRX_PIN			0
#define HW_CANTX_PORT			GPIOD
#define HW_CANTX_PIN			1

// Measurement macros
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_L4				ADC_Value[ADC_IND_SENS4]
#define ADC_V_L5				ADC_Value[ADC_IND_SENS5]
#define ADC_V_L6				ADC_Value[ADC_IND_SENS6]

#define ADC_V_ZERO				(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

#define READ_HALL1_2()			palReadPad(HW_HALL_ENC_GPIO4, HW_HALL_ENC_PIN4)
#define READ_HALL2_2()			palReadPad(HW_HALL_ENC_GPIO5, HW_HALL_ENC_PIN5)
#define READ_HALL3_2()			palReadPad(HW_HALL_ENC_GPIO6, HW_HALL_ENC_PIN6)

// Note: the unity seems to trigger this fault randomly sometimes, which can be
// dangerous. Therefore it is disabled, and we rely on the DRV to limit the current
// if needed.
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT		401.0	// The maximum absolute current above which a fault is generated
#endif

// Setting limits
#ifdef HW_HAS_DUAL_PARALLEL
#define HW_LIM_CURRENT				-300.0, 300.0
#define HW_LIM_CURRENT_ABS			800.0, 804.0
#define HW_LIM_CURRENT_IN		-200.0, 200.0
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT	400.0	// The maximum absolute current above which a fault is generated
#define MCCONF_FOC_OFFSETS_CURRENT_0	4096.0 // Current 0 offset
#define MCCONF_FOC_OFFSETS_CURRENT_1	4096.0 // Current 1 offset
#define MCCONF_FOC_OFFSETS_CURRENT_2	4096.0 // Current 2 offset
#endif
#else
#define HW_LIM_CURRENT				-150.0, 150.0
#define HW_LIM_CURRENT_ABS		400.0, 402.0
#define HW_LIM_CURRENT_IN		-100.0, 100.0
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT	200.0	// The maximum absolute current above which a fault is generated
#endif
#endif

#define HW_LIM_VIN				6.0, 59.0
#define HW_LIM_ERPM				-200e3, 200e3
#define HW_LIM_DUTY_MIN			0.0, 0.1
#define HW_LIM_DUTY_MAX			0.0, 0.95
#define HW_LIM_TEMP_FET			-40.0, 120.0

// Functions
void smart_switch_thread_start(void);
void smart_switch_pin_init(void);
bool smart_switch_is_pressed(void);
void smart_switch_shut_down(void);
void smart_switch_keep_on(void);

#endif /* HW_UNITY_H_ */
