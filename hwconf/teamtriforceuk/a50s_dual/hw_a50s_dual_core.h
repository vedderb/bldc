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

#ifndef HW_A50S_DUAL_CORE_H_
#define HW_A50S_DUAL_CORE_H_

// HW properties
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_FILTERS
#define HW_USE_25MHZ_EXT_CLOCK

// Macros

// ADC Vectors, see .c
#define HW_ADC_CHANNELS			9
#define HW_ADC_INJ_CHANNELS		3
#define HW_ADC_NBR_CONV			3

// ADC Indexes - refer to .c for descriptions
#define ADC_IND_CURR1			0
#define ADC_IND_CURR2			1
#define ADC_IND_CURR3			2


#define ADC_IND_SENS1			3
#define ADC_IND_SENS2			4
#define ADC_IND_SENS3			5

#define ADC_IND_VIN_SENS		6

#define ADC_IND_TEMP_MOS		8


// not connected but required to keep compiler happy
#define ADC_IND_EXT				7
#define ADC_IND_TEMP_MOTOR		7



// ADC macros and settings

// Component parameters (can be overridden)


#ifndef V_REG
#define V_REG						3.29
#endif
#ifndef VIN_R1
#define VIN_R1						47000.0
#endif
#ifndef VIN_R2
#define VIN_R2						2200.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN			50.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES			0.0005    
#endif

#define CURRENT_CAL1 				get_cal1()
#define CURRENT_CAL2 				get_cal2()
#define CURRENT_CAL3 				get_cal3()

// Input voltage
#define GET_INPUT_VOLTAGE()			((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
#define NTC_RES(adc_val)			((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind)			(1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)		(10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)		25

// Voltage on ADC channel
#define ADC_VOLTS(ch)				((float)ADC_Value[ch] / 4096.0 * V_REG)

// LEDs
#define LED_GREEN_GPIO				GPIOD
#define LED_GREEN_PIN				7
#define LED_RED_GPIO				GPIOB
#define LED_RED_PIN					4

#define LED_GREEN_ON()				palSetPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_GREEN_OFF()				palClearPad(LED_GREEN_GPIO, LED_GREEN_PIN)
#define LED_RED_ON()				palSetPad(LED_RED_GPIO, LED_RED_PIN)
#define LED_RED_OFF()				palClearPad(LED_RED_GPIO, LED_RED_PIN)

// Phase filter
#define PHASE_FILTER_OFF()			palSetPad(GPIOE, 11); palSetPad(GPIOE, 9); palSetPad(GPIOB, 2)
#define PHASE_FILTER_ON()			palClearPad(GPIOE, 11); palClearPad(GPIOE, 9); palClearPad(GPIOB, 2)

// Pins for ID detection
#define HW_DEFAULT_ID           	get_a50s_dual_id()

// UART Peripheral
#define HW_UART_DEV					SD3
#define HW_UART_GPIO_AF				GPIO_AF_USART3
#define HW_UART_TX_PORT				GPIOB
#define HW_UART_TX_PIN				10
#define HW_UART_RX_PORT				GPIOD
#define HW_UART_RX_PIN				9

// SPI pins - not used
#define HW_SPI_DEV					SPID1
#define HW_SPI_GPIO_AF				GPIO_AF_SPI1
#define HW_SPI_PORT_NSS				GPIOB
#define HW_SPI_PIN_NSS				11
#define HW_SPI_PORT_SCK				GPIOA
#define HW_SPI_PIN_SCK				5
#define HW_SPI_PORT_MOSI			GPIOA
#define HW_SPI_PIN_MOSI				7
#define HW_SPI_PORT_MISO			GPIOA
#define HW_SPI_PIN_MISO				6

// I2C Peripheral - not used
#define HW_USE_I2CD1
#define HW_I2C_DEV					I2CD1
#define HW_I2C_GPIO_AF				GPIO_AF_I2C1
#define HW_I2C_SCL_PORT				GPIOB
#define HW_I2C_SCL_PIN				6
#define HW_I2C_SDA_PORT				GPIOB
#define HW_I2C_SDA_PIN				7

// Hall/encoder pins - not used
#define HW_HALL_ENC_GPIO1			GPIOC
#define HW_HALL_ENC_PIN1			6
#define HW_HALL_ENC_GPIO2			GPIOC
#define HW_HALL_ENC_PIN2			7
#define HW_HALL_ENC_GPIO3			GPIOC
#define HW_HALL_ENC_PIN3			8
#define HW_ENC_TIM					TIM3
#define HW_ENC_TIM_AF				GPIO_AF_TIM3
#define HW_ENC_TIM_CLK_EN()			RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE)
#define HW_ENC_EXTI_PORTSRC			EXTI_PortSourceGPIOC
#define HW_ENC_EXTI_PINSRC			EXTI_PinSource8
#define HW_ENC_EXTI_CH				EXTI9_5_IRQn
#define HW_ENC_EXTI_LINE			EXTI_Line8
#define HW_ENC_EXTI_ISR_VEC			EXTI9_5_IRQHandler
#define HW_ENC_TIM_ISR_CH			TIM3_IRQn
#define HW_ENC_TIM_ISR_VEC			TIM3_IRQHandler

// ICU Peripheral for servo decoding - not used
#define HW_USE_SERVO_TIM4
#define HW_ICU_TIMER				TIM4
#define HW_ICU_TIM_CLK_EN()			RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ICU_DEV					ICUD4
#define HW_ICU_CHANNEL				ICU_CHANNEL_1
#define HW_ICU_GPIO_AF				GPIO_AF_TIM4
#define HW_ICU_GPIO					GPIOB
#define HW_ICU_PIN					6

// Measurement macros
#define ADC_V_L1					ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2					ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3					ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO					(ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()				palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()				palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()				palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Override dead time. See the stm32f4 reference manual for calculating this value.
#define HW_DEAD_TIME_NSEC			0.0		// FD6288Q has 200ns built in deadtime

// Default setting overrides

#define MCCONF_L_MIN_VOLTAGE		8.0	// Minimum input voltage

#define MCCONF_DEFAULT_MOTOR_TYPE	MOTOR_TYPE_FOC

#define MCCONF_FOC_F_ZV				25000.0 // Switching frequency reduced to allow rise time of low side shunts

#define HW_LIM_FOC_CTRL_LOOP_FREQ	5000.0, 25000.0	// Limit to 50kHz max 

#define MCCONF_FOC_DT_US			0.0 // Microseconds for dead time compensation

#define MCCONF_M_MOTOR_TEMP_SENS_TYPE TEMP_SENSOR_DISABLED // Motor Temperature Sensor Type
// Only use phase filters for detection by default to get good resistance measurement. 
// In testing I found that phase filters gave worse startup
#define MCCONF_FOC_PHASE_FILTER_MAX_ERPM	10.0 

// Use full clarke transform by default
#define MCCONF_FOC_CURRENT_SAMPLE_MODE FOC_CURRENT_SAMPLE_MODE_ALL_SENSORS
#define MCCONF_FOC_OFFSETS_CAL_ON_BOOT	true // cal on boot to account for temperature changes

#define MCCONF_L_MAX_ABS_CURRENT	60.0	// The maximum absolute current above which a fault is generated
#define MCCONF_FOC_SAMPLE_V0_V7		false	// Run control loop in both v0 and v7 (requires phase shunts)
#define MCCONF_L_CURRENT_MAX		40.0   // Current limit in Amperes (Upper)
#define MCCONF_L_CURRENT_MIN		-40.0	// Current limit in Amperes (Lower)
#define MCCONF_L_IN_CURRENT_MAX		30.0	// Input current limit in Amperes (Upper)
#define MCCONF_L_IN_CURRENT_MIN		-30.0	// Input current limit in Amperes (Lower)

// Defaults for BLDC
#define MCCONF_M_BLDC_F_SW_MIN			10000 // Minimum switching frequency in bldc mode
#define MCCONF_SL_MIN_ERPM_CYCLE_INT_LIMIT	4000.0	// Minimum RPM to calculate the BEMF coupling from

// Setting limits
#define HW_LIM_CURRENT				-50.0, 50.0 
#define HW_LIM_CURRENT_IN			-40.0, 40.0
#define HW_LIM_CURRENT_ABS			0.0, 60.0
#define HW_LIM_ERPM					-200e3, 200e3
#define HW_LIM_DUTY_MIN				0.0, 0.1
#define HW_LIM_DUTY_MAX				0.0, 0.98	
#define HW_LIM_TEMP_FET				-40.0, 100.0

#ifdef HW_A50S_20S
#define HW_LIM_VIN					4.0, 90.0
#define MCCONF_L_MAX_VOLTAGE		86	// Maximum input voltage
#elif defined (HW_A50S_12S)
#define HW_LIM_VIN					4.0, 56.0
#define MCCONF_L_MAX_VOLTAGE		55	// Maximum input voltage
#elif defined (HW_A50S_8S)
#define HW_LIM_VIN					4.0, 37.0
#define MCCONF_L_MAX_VOLTAGE		36	// Maximum input voltage
#elif defined (HW_A50S_6S)
#define HW_LIM_VIN					4.0, 28.0
#define MCCONF_L_MAX_VOLTAGE		26	// Maximum input voltage
#else
#error "Must define a hardware type"
#endif

void hw_a50s_get_id_from_pins(void);
uint8_t get_a50s_dual_id(void);
float get_cal1(void);
float get_cal2(void);
float get_cal3(void);

#endif /* HW_A50S_DUAL_CORE_H_ */
