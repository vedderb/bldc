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
#ifndef HW_VESC_DUET_H_
#define HW_VESC_DUET_H_

#define HW_HAS_DUAL_MOTORS

#define HW_NAME                 "VESC_DUET"

#ifndef HW_NAME
#error "Must define hardware type"
#endif

#define INVERTED_SHUNT_POLARITY
#define HW_HAS_3_SHUNTS

#define HW_DEAD_TIME_NSEC               200.0

//Switch Pins
#define HW_HAS_STORMCORE_SWITCH
#define HW_HAS_RGB_SWITCH

#define SWITCH_IN_GPIO					GPIOA
#define SWITCH_IN_PIN					15
#define SWITCH_OUT_GPIO					GPIOB
#define SWITCH_OUT_PIN					13
#define SWITCH_PRECHARGED_GPIO			GPIOE
#define SWITCH_PRECHARGED_PIN			2
#define SWITCH_LED_3_GPIO				GPIOD
#define SWITCH_LED_3_PIN				11
#define SWITCH_LED_2_GPIO				GPIOD
#define SWITCH_LED_2_PIN				10
#define SWITCH_LED_1_GPIO				GPIOD
#define SWITCH_LED_1_PIN				15

#define LED_PWM1_ON()			palClearPad(SWITCH_LED_1_GPIO,SWITCH_LED_1_PIN)
#define LED_PWM1_OFF()			palSetPad(SWITCH_LED_1_GPIO,SWITCH_LED_1_PIN)
#define LED_PWM2_ON()			palClearPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)
#define LED_PWM2_OFF()			palSetPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)
#define LED_PWM3_ON()			palClearPad(SWITCH_LED_3_GPIO, SWITCH_LED_3_PIN)
#define LED_PWM3_OFF()			palSetPad(SWITCH_LED_3_GPIO, SWITCH_LED_3_PIN)

#define SMART_SWITCH_MSECS_PRESSED_OFF		2000

#define HW_HAS_PHASE_FILTERS
#define PHASE_FILTER_OFF()               palSetPad(GPIOE, 3); palSetPad(GPIOE, 4); palSetPad(GPIOE, 6);
#define PHASE_FILTER_ON()              palClearPad(GPIOE, 3); palClearPad(GPIOE, 4); palClearPad(GPIOE, 6);
#define PHASE_FILTER_OFF_M2()            palSetPad(GPIOE, 0); palSetPad(GPIOE, 1); palSetPad(GPIOE, 2);
#define PHASE_FILTER_ON_M2()           palClearPad(GPIOE, 0); palClearPad(GPIOE, 1); palClearPad(GPIOE, 2);

// Output1
#define OUT_1_GPIO		    GPIOE
#define OUT_1_PIN			14
#define OUT_1_ON()		    palSetPad(OUT_1_GPIO, OUT_1_PIN)
#define OUT_1_OFF()		    palClearPad(OUT_1_GPIO, OUT_1_PIN)

// Output2
#define OUT_2_GPIO			GPIOE
#define OUT_2_PIN			15
#define OUT_2_ON()			palSetPad(OUT_2_GPIO, OUT_2_PIN)
#define OUT_2_OFF()		    palClearPad(OUT_2_GPIO, OUT_2_PIN)


#define HW_SHUTDOWN_HOLD_ON();
#define HW_SAMPLE_SHUTDOWN()		1
#define HW_SHUTDOWN_HOLD_OFF()		palClearPad(SWITCH_OUT_GPIO, SWITCH_OUT_PIN); \
		palClearPad(SWITCH_PRECHARGED_GPIO, SWITCH_PRECHARGED_PIN);


#define DCCAL_ON()
#define DCCAL_OFF()

#define HW_EARLY_INIT()				smart_switch_pin_init(); \
									smart_switch_thread_start();


//Pins for BLE UART
//#define USE_ALT_UART_PORT

#define HW_UART_P_BAUD				115200
#define HW_UART_P_DEV				SD1
#define HW_UART_P_GPIO_AF			GPIO_AF_USART1
#define HW_UART_P_TX_PORT			GPIOA
#define HW_UART_P_TX_PIN			9
#define HW_UART_P_RX_PORT			GPIOA
#define HW_UART_P_RX_PIN			10

#define ADC_SW_EN_PORT		GPIOB
#define ADC_SW_EN_PIN		12
#define ADC_SW_1_PORT		GPIOD
#define ADC_SW_1_PIN		7
#define ADC_SW_2_PORT		GPIOB
#define ADC_SW_2_PIN		3
#define ADC_SW_3_PORT		GPIOE
#define ADC_SW_3_PIN		7

#define AD_DIS()	palSetPad(ADC_SW_EN_PORT, ADC_SW_EN_PIN )
#define AD1_L()		palClearPad(ADC_SW_1_PORT, ADC_SW_1_PIN )
#define AD1_H()		palSetPad(ADC_SW_1_PORT, ADC_SW_1_PIN )
#define AD2_L()		palClearPad(ADC_SW_2_PORT, ADC_SW_2_PIN )
#define AD2_H()		palSetPad(ADC_SW_2_PORT, ADC_SW_2_PIN )
#define AD3_L()		palClearPad(ADC_SW_3_PORT, ADC_SW_3_PIN )
#define AD3_H()		palSetPad(ADC_SW_3_PORT, ADC_SW_3_PIN )
#define AD_EN()		palClearPad(ADC_SW_EN_PORT, ADC_SW_EN_PIN )

#define ENABLE_MOS_TEMP1()			AD_DIS();	AD3_L();	AD2_L();	AD1_L();	AD_EN();
#define ENABLE_MOS_TEMP2()			AD_DIS();	AD3_L();	AD2_L();	AD1_H();	AD_EN();
#define ENABLE_MOT_TEMP1()			AD_DIS();	AD3_L();	AD2_H();	AD1_L();	AD_EN();
#define ENABLE_MOT_TEMP2()          AD_DIS();	AD3_L();	AD2_H();	AD1_H();	AD_EN();
#define ENABLE_ADC_EXT_2()          AD_DIS();	AD3_H();	AD2_L();	AD1_L();	AD_EN();
#define ENABLE_ADC_EXT_1()          AD_DIS();	AD3_H();	AD2_L();	AD1_H();	AD_EN();
#define ENABLE_ADC_EXT_3()          AD_DIS();	AD3_H();	AD2_H();	AD1_L();	AD_EN();
#define ENABLE_V_BATT_DIV()         AD_DIS();	AD3_H();	AD2_H();	AD1_H();	AD_EN();


#define LED_GREEN_ON()				palSetPad(GPIOC, 9);// palClearPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN);
#define LED_GREEN_OFF()				palClearPad(GPIOC, 9);// palSetPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN);
#define LED_RED_ON()				palSetPad(GPIOA, 8); //palClearPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN);
#define LED_RED_OFF()				palClearPad(GPIOA, 8); //palSetPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN);
#define LED_SWITCH_R_ON()			palClearPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN)
#define LED_SWITCH_R_OFF()			palSetPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN)
#define LED_SWITCH_G_ON()			palClearPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)
#define LED_SWITCH_G_OFF()			palSetPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)
#define LED_SWITCH_B_ON()			palClearPad(SWITCH_LED_1_GPIO, SWITCH_LED_1_PIN)
#define LED_SWITCH_B_OFF()			palSetPad(SWITCH_LED_1_GPIO, SWITCH_LED_1_PIN)


/*
 * ADC Vector
 *
 * 0:    IN9      CURR1
 * 1:    IN8      CURR2
 * 2:    IN10     V BUS DIV
 * 3:    IN14     CURR4
 * 4:    IN15     CURR3
 * 5:    IN3      VM_SENS (12V)
 * 6:    IN5      CURR6
 * 7:    IN6      CURR5
 * 8:    IN13     SENS4
 * 9:    IN4      ADC_MUX
 * 10:   IN12     SENS5
 * 11:   IN11     SENS6
 * 12:   IN0      SENS2
 * 13:   IN1      SENS3
 * 14:   IN2      SENS1
 */

#define HW_ADC_CHANNELS			15
#define HW_ADC_CHANNELS_EXTRA	15
#define HW_ADC_INJ_CHANNELS		2
#define HW_ADC_NBR_CONV			5

// ADC Indexes

#define ADC_IND_CURR1			0
#define ADC_IND_CURR2			1
#define ADC_IND_VIN_SENS		2

#define ADC_IND_CURR3			3
#define ADC_IND_CURR4			4
#define ADC_IND_ADC_MUX			5

#define ADC_IND_CURR6			6
#define ADC_IND_CURR5			7
#define ADC_IND_SENS4			8

#define ADC_IND_VM_SENSE		9
#define ADC_IND_SENS5			10
#define ADC_IND_SENS6			11

#define ADC_IND_SENS2			12
#define ADC_IND_SENS3			13
#define ADC_IND_SENS1			14

#define ADC_IND_TEMP_MOS		15
#define ADC_IND_TEMP_MOS_M2		16
#define ADC_IND_TEMP_MOTOR		17
#define ADC_IND_TEMP_MOTOR_2	18
#define ADC_IND_EXT				19
#define ADC_IND_EXT2			20
#define ADC_IND_EXT3			21
#define ADC_IND_V_BATT			22

// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.3
#endif
#ifndef VIN_R1
#define VIN_R1					68000.0
#endif
#ifndef VIN_R2
#define VIN_R2					2200.0
#endif


#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN        20.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES       0.0005
#endif

#define VBATT_R1					360000.0
#define VBATT_R2					10000.0
// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))
#define GET_BATT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_V_BATT] * ((VBATT_R1 + VBATT_R2) / VBATT_R2))
#define GET_VM_SENSE_VOLTAGE()	((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VM_SENSE] * ((VIN_R1 + VIN_R2) / VIN_R2))

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4095.0 * V_REG)

// NTC Termistors
#define NTC_RES(adc_val)		(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side // High side ->((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind)		(1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3434.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)
#define NTC_TEMP_MOTOR_2(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR_2]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

// Double samples in beginning and end for positive current measurement.
// Useful when the shunt sense traces have noise that causes offset.
#ifndef CURR1_DOUBLE_SAMPLE
#define CURR1_DOUBLE_SAMPLE		0
#endif
#ifndef CURR2_DOUBLE_SAMPLE
#define CURR2_DOUBLE_SAMPLE		0
#endif

// Number of servo outputs
#define HW_SERVO_NUM			2

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
// NRF pins
#define NRF_PORT_CSN            GPIOD
#define NRF_PIN_CSN             3
#define NRF_PORT_SCK            GPIOD
#define NRF_PIN_SCK             2
#define NRF_PORT_MOSI           GPIOD
#define NRF_PIN_MOSI            11
#define NRF_PORT_MISO           GPIOD
#define NRF_PIN_MISO            10

// NRF SWD
#define NRF5x_SWDIO_GPIO        GPIOD
#define NRF5x_SWDIO_PIN         9
#define NRF5x_SWCLK_GPIO        GPIOD
#define NRF5x_SWCLK_PIN         8

#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE	MOTOR_TYPE_FOC
#endif

// LSM6DS3
#define LSM6DS3_SDA_GPIO		GPIOB
#define LSM6DS3_SDA_PIN			9
#define LSM6DS3_SCL_GPIO		GPIOB
#define LSM6DS3_SCL_PIN			8

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

//CAN
#define HW_CANRX_PORT			GPIOD
#define HW_CANRX_PIN			0
#define HW_CANTX_PORT			GPIOD
#define HW_CANTX_PIN			1

// Setting limits
#define HW_LIM_CURRENT				-120.0, 120.0
#define HW_LIM_CURRENT_ABS			0.0, 160.0
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT	150.0	// The maximum absolute current above which a fault is generated
#endif

#define HW_LIM_CURRENT_IN			-100.0, 100.0
#define HW_LIM_VIN					6.0, 94.0
#define HW_LIM_ERPM					-200e3, 200e3
#define HW_LIM_DUTY_MIN				0.0, 0.1
#define HW_LIM_DUTY_MAX				0.0, 0.95
#define HW_LIM_TEMP_FET				-40.0, 120.0

// Functions
void smart_switch_thread_start(void);
void smart_switch_pin_init(void);
bool smart_switch_is_pressed(void);
void smart_switch_shut_down(void);
void smart_switch_keep_on(void);

#endif /* HW_VESC_DUET_H_ */
