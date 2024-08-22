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

#ifndef HW_A200S_V41_CORE_H_
#define HW_A200S_V41_CORE_H_

// HW properties
#define HW_HAS_3_SHUNTS
#define INVERTED_SHUNT_POLARITY
#define HW_HAS_PHASE_FILTERS
#define HW_USE_25MHZ_EXT_CLOCK
#define HW_USE_ALTERNATIVE_DC_CAL

#define HW_ADC_CHANNELS			15
#define HW_ADC_INJ_CHANNELS		3
#define HW_ADC_NBR_CONV			5

// ADC Indexes - refer to .c for descriptions
#define ADC_IND_CURR1			0
#define ADC_IND_CURR2			1
#define ADC_IND_CURR3			2

#define ADC_IND_VIN_SENS		8
#define ADC_IND_SENS1			3
#define ADC_IND_SENS2			4
#define ADC_IND_SENS3			5

#define ADC_IND_EXT				6
#define ADC_IND_EXT2			7
//#define ADC_IND_EXT3			

#define ADC_IND_TEMP_MOS		11
#define ADC_IND_TEMP_MOS_2		12
#define ADC_IND_TEMP_MOS_3		13
#define ADC_IND_TEMP_MOTOR		10

#define ADC_IND_VREFINT			9


// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG					3.30
#endif
#ifndef VIN_R1
#define VIN_R1					100000.0
#endif
#ifndef VIN_R2
#define VIN_R2					3160.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN		20.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES		(0.0002 / 3)
#endif


// Input voltage
#define GET_INPUT_VOLTAGE()		((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
#define NTC_RES(adc_val)		(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Mos temp sensor on low side

#define NTC_RES_MOTOR(adc_val)	(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)	(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)

#define NTC_TEMP(adc_ind)		hw_a200s_get_temp()
#define NTC_TEMP_MOS1()			(1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)
#define NTC_TEMP_MOS2()			(1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS_2]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)
#define NTC_TEMP_MOS3()			(1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS_3]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4096.0 * V_REG)


#define LED_GREEN_GPIO			GPIOB
#define LED_GREEN_PIN			5
#define LED_RED_GPIO			GPIOB
#define LED_RED_PIN				7

#define LED_GREEN_ON()			palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF()			palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON()			palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF()			palClearPad(LED_RED_GPIO, LED_RED_PIN)

// Aux - TCKE812NA,RF - Aux output controlled by I2C
#define AUX_ON()				hw_a200s_aux(1)
#define AUX_OFF()				hw_a200s_aux(0)

// Phase filter
#define PHASE_FILTER_OFF()		palSetPad(GPIOC, 9); palSetPad(GPIOC, 13); palSetPad(GPIOC, 14)
#define PHASE_FILTER_ON()		palClearPad(GPIOC, 9); palClearPad(GPIOC, 13); palClearPad(GPIOC, 14)

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOA
#define HW_ADC_EXT_PIN			5
#define HW_ADC_EXT2_GPIO		GPIOA
#define HW_ADC_EXT2_PIN			6

// UART Peripheral - Comm port
#define HW_UART_DEV				SD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOB
#define HW_UART_TX_PIN			10
#define HW_UART_RX_PORT			GPIOB
#define HW_UART_RX_PIN			11

// SPI pins
#define HW_SPI_DEV				SPID1
#define HW_SPI_GPIO_AF			GPIO_AF_SPI1
#define HW_SPI_PORT_NSS			GPIOB
#define HW_SPI_PIN_NSS			11
#define HW_SPI_PORT_SCK			GPIOA
#define HW_SPI_PIN_SCK			5
#define HW_SPI_PORT_MOSI		GPIOA
#define HW_SPI_PIN_MOSI			7
#define HW_SPI_PORT_MISO		GPIOA
#define HW_SPI_PIN_MISO			6

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

// Permanent UART Peripheral (for Logger)
#define HW_UART_P_BAUD			100000
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

// I2C for IMU, logger and output control
#define LSM6DS3_SDA_GPIO		GPIOC
#define LSM6DS3_SDA_PIN			12
#define LSM6DS3_SCL_GPIO		GPIOA
#define LSM6DS3_SCL_PIN			4
#define ATTINY1616_ADDR  		0X68 // Low side body diode OC and logger
#define ATTINY3216_ADDR  		0X69 // Low side Mosfet Channel OC and Aux output control
#define TPL0401A_10DCKR_ADDR  	0X2E // Low side Mosfet Channel OC limit digipot
#define TPL0401B_10DCKR_ADDR	0X3E // Low side body diode OC limit digipot
#define ATTINY3216_HANDSHAKE_REPLY 0x71
#define ATTINY1616_HANDSHAKE_REPLY 0x73


// Hardware protection
#define HW_RESET_DRV_FAULTS()	hw_a200s_reset_faults()
#define ENABLE_GATE()			palClearPad(GPIOC, 5)
#define DISABLE_GATE()			palSetPad(GPIOC, 5)
#define IS_DRV_FAULT()			hw_a200s_drv_fault_check() 
#define HW_PROTECTION_CURR_TRIP_CHANNEL 	900.0f 
#define HW_PROTECTION_CURR_TRIP_DIODE 		1000.0f // Seems to be about 45% of this (i.e. this trips with ~450A flowing before the fet turns off)

#define BRK_GPIO				GPIOB
#define BRK_PIN					12

// Measurement macros
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO				(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Default setting overrides
#define MCCONF_L_MIN_VOLTAGE			14.0		// Minimum input voltage

#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC

#define MCCONF_FOC_F_ZV					30000.0

#define HW_LIM_FOC_CTRL_LOOP_FREQ		5000.0, 25000.0	//Limit to 50kHz max

#define MCCONF_L_MAX_ABS_CURRENT		200.0	// The maximum absolute current above which a fault is generated

#define MCCONF_FOC_SAMPLE_V0_V7			false	// Run control loop in both v0 and v7 (requires phase shunts)

#define MCCONF_L_CURRENT_MAX			100.0	// Current limit in Amperes (Upper)

#define MCCONF_L_CURRENT_MIN			-100.0	 // Current limit in Amperes (Lower)

#define MCCONF_L_SLOW_ABS_OVERCURRENT	false	// Use the raw current for the overcurrent fault detection

#define MCCONF_L_IN_CURRENT_MAX			20.0	// Input current limit in Amperes (Upper)

#define MCCONF_L_IN_CURRENT_MIN			-5.0	// Input current limit in Amperes (Lower)

#define MCCONF_M_MOTOR_TEMP_SENS_TYPE TEMP_SENSOR_DISABLED // Motor Temperature Sensor Type

#define MCCONF_FOC_OFFSETS_CAL_ON_BOOT	false // Don't Measure offsets every boot, it is done once at motor setup

// Setting limits
#define HW_LIM_CURRENT			-300.0, 300.0	
#define HW_LIM_CURRENT_IN		-180.0, 180.0  
#define HW_LIM_CURRENT_ABS		0.0, 450.0
#define HW_LIM_VIN				-1.0, 90.0			  
#define HW_LIM_ERPM				-200e3, 200e3
#define HW_LIM_DUTY_MIN			0.0, 0.1
#define HW_LIM_DUTY_MAX			0.0, 0.95
#define HW_LIM_TEMP_FET			-40.0, 110.0
#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE	20.0 * 4.2 + 5.0	// Maximum input voltage
#endif
#ifndef MCCONF_FOC_DT_US
#define MCCONF_FOC_DT_US		0.1 // Microseconds for dead time compensation
#endif

#define HW_DEAD_TIME_NSEC		700.0 
#define HW_NAME					"A200S_V41"
 

// HW-specific functions
float hw_a200s_get_temp(void);
void hw_a200s_reset_faults(void);
bool hw_a200s_hardware_handshake(void);
bool hw_a200s_drv_fault_check(void);
void hw_a200s_aux(bool);

#endif /* HW_A200S_V41_CORE_H_ */
