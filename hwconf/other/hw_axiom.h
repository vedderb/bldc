/*
	Copyright 2017 Benjamin Vedder	benjamin@vedder.se
	Copyright 2019 Marcos Chaparro	mchaparro@powerdesigns.ca

	For support, please contact www.powerdesigns.ca

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

#ifndef HW_AXIOM_H_
#define HW_AXIOM_H_

#define HW_NAME					"AXIOM"

//#define HW_AXIOM_USE_DAC
//#define HW_AXIOM_USE_MOTOR_TEMP
#define HW_HAS_INPUT_CURRENT_SENSOR
#define HW_USE_LINE_TO_LINE
#define	HW_AXIOM_FORCE_HIGH_CURRENT_MEASUREMENTS
#define HW_VERSION_AXIOM

// HW properties
#define HW_HAS_3_SHUNTS
#define HW_HAS_PHASE_SHUNTS
#define HW_HAS_GATE_DRIVER_SUPPLY_MONITOR

// Macros
#define ENABLE_GATE()			palSetPad(GPIOH, 14)
#define DISABLE_GATE()			palClearPad(GPIOH, 14)
#define DCCAL_ON()
#define DCCAL_OFF()
#define IS_DRV_FAULT()			(!palReadPad(GPIOB, 12))

#define LED_GREEN_ON()			palSetPad(GPIOB, 2)
#define LED_GREEN_OFF()			palClearPad(GPIOB, 2)

#ifdef HW_PALTA_REV_B
#define LED_RED_ON()			palSetPad(GPIOB, 1)
#define LED_RED_OFF()			palClearPad(GPIOB, 1)
#else
#define LED_RED_ON()			palSetPad(GPIOB, 11)
#define LED_RED_OFF()			palClearPad(GPIOB, 11)
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
 * 6  (1):  IN8		ADC_IND_EXT2
 * 7  (2):	IN6		TEMP_IGBT_2
 * 8  (3):	IN3		TEMP_PCB
 * 9  (1):	IN14	TEMP_MOTOR
 * 10 (2):	IN15	ADC_IND_EXT
 * 11 (3):	IN13	AN_IN
 * 12 (1):	IN9		V_GATE_DRIVER
 * 13 (2):	IN4		TEMP_IGBT_1
 * 14 (3)	IN3		UNUSED
 * 15 (1):	IN5		TEMP_IGBT_3
 * 16 (2):	VREFINT	ADC_IND_VREFINT
 * 17 (3):	VREFINT	UNUSED
 */

#define HW_ADC_CHANNELS					18
#define HW_ADC_INJ_CHANNELS				3
#define HW_ADC_NBR_CONV					6

// ADC Indexes
#define ADC_IND_SENS1					0
#define ADC_IND_SENS2					1
#define ADC_IND_SENS3					2
#define ADC_IND_CURR1					3
#define ADC_IND_CURR2					4
#define ADC_IND_CURR3					5
#define ADC_IND_VIN_SENS				11
#define ADC_IND_VOUT_GATE_DRV			12
#define ADC_IND_EXT						10
#define ADC_IND_EXT2					6
#define ADC_IND_TEMP_PCB				8
#define ADC_IND_TEMP_MOTOR				9
#define ADC_IND_TEMP_IGBT_1				13
#define ADC_IND_TEMP_IGBT_2				7
#define ADC_IND_TEMP_IGBT_3				15
#define ADC_IND_VREFINT					16

// When reading switch temperature, return the center IGBT temp
// because it will be the hotter one.
#define ADC_IND_TEMP_MOS				ADC_IND_TEMP_IGBT_2

// ADC macros and settings

#ifdef HW_PALTA_REV_B
#define HVDC_TRANSFER_FUNCTION			112.15			//[V/V]
#define PHASE_VOLTAGE_TRANSFER_FUNCTION	112.15			//[V/V]
#else
#define HVDC_TRANSFER_FUNCTION			196.0			//[V/V]
#define PHASE_VOLTAGE_TRANSFER_FUNCTION	367.7			//[V/V]
#endif
#define DEFAULT_CURRENT_AMP_GAIN		0.001035	//Transfer Function [V/A]
//#define DEFAULT_CURRENT_AMP_GAIN		0.003761	//Transfer Function [V/A] for ISB-425-A
//#define DEFAULT_CURRENT_AMP_GAIN		0.001249	//Transfer Function [V/A] for HTFS 800-P
//#define DEFAULT_CURRENT_AMP_GAIN		0.004994	//Transfer Function [V/A] for HASS 100-S
//#define DEFAULT_CURRENT_AMP_GAIN		0.001249	//Transfer Function [V/A] for HASS 400-S
//#define DEFAULT_CURRENT_AMP_GAIN		0.0008324	//Transfer Function [V/A] for HASS 600-S

#define DEFAULT_INPUT_CURRENT_AMP_GAIN		0.00104069	//Transfer Function [V/A] 

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG							3.3
#endif
#ifndef VIN_R1
#define VIN_R1							(PHASE_VOLTAGE_TRANSFER_FUNCTION - 1.0)
#endif
#ifndef VIN_R2
#define VIN_R2							1.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN				hw_axiom_get_current_sensor_gain()
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES				1.0 // Unity gain so we use a single transfer function defined as CURRENT_AMP_GAIN
#endif

#define HW_MAX_CURRENT_OFFSET				620		// More than this offset (0.5 Vdc) trips the offset fault (likely a sensor disconnected)
#define MCCONF_MAX_CURRENT_UNBALANCE		130.0	// [Amp] More than this unbalance trips the fault (likely a sensor disconnected)
#define MCCONF_MAX_CURRENT_UNBALANCE_RATE	0.3		// Fault if more than 30% of the time the motor is unbalanced

// Input voltage
#define GET_INPUT_VOLTAGE()				((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * (HVDC_TRANSFER_FUNCTION))

//Input current
#define GET_INPUT_CURRENT()				hw_axiom_read_input_current()
#define GET_INPUT_CURRENT_OFFSET()		hw_axiom_get_input_current_offset()
#define MEASURE_INPUT_CURRENT_OFFSET()	hw_axiom_start_input_current_sensor_offset_measurement()

// NTC Termistors
#define NTC_RES(adc_val)				((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_RES_IGBT(adc_val)			((4095.0 * 8870.0 * 2) / adc_val - 18870.0)
#define NTC_TEMP(adc_ind)				hw_axiom_get_highest_IGBT_temp()
//#define NTC_TEMP(adc_ind)				(1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3434.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)			hw_axiom_NTC_res_motor_filter(adc_val)

// If DAC enabled, only IGBT_TEMP_3 is available
#ifdef HW_AXIOM_USE_DAC
#define NTC_TEMP_MOS1()			(25.0)
#define NTC_TEMP_MOS2()			(25.0)
#define NTC_TEMP_MOS3()			hw_axiom_temp_sensor_filter(3)
#else
// Individual IGBT Temperature sensing
#define NTC_TEMP_MOS1()			hw_axiom_temp_sensor_filter(1)
#define NTC_TEMP_MOS2()			hw_axiom_temp_sensor_filter(2)
#define NTC_TEMP_MOS3()			hw_axiom_temp_sensor_filter(3)
#endif

#ifdef HW_AXIOM_USE_MOTOR_TEMP
#define NTC_TEMP_MOTOR(beta)			(1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)
#else
#define NTC_TEMP_MOTOR(beta)			25.0
#endif
// Voltage on ADC channel
#define ADC_VOLTS(ch)					((float)ADC_Value[ch] / 4096.0 * V_REG)

// Sin/Cos Encoder signals
#define ENCODER_SIN_VOLTS				ADC_VOLTS(ADC_IND_EXT)
#define ENCODER_COS_VOLTS				ADC_VOLTS(ADC_IND_EXT2)

#ifdef HW_PALTA_REV_B
#define GET_GATE_DRIVER_SUPPLY_VOLTAGE()	15.0
#else
// Gate driver power supply output voltage
#define GET_GATE_DRIVER_SUPPLY_VOLTAGE()	((float)ADC_VOLTS(ADC_IND_VOUT_GATE_DRV) * 11.0)
#endif

#define ANGLE_TO_DAC_VALUE(angle)		( angle * 512.0 + 0x800 )//angle between -pi to pi

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

// COMM-port ADC GPIOs
#define HW_ADC_EXT_GPIO			GPIOC
#define HW_ADC_EXT_PIN			5
#define HW_ADC_EXT2_GPIO		GPIOB
#define HW_ADC_EXT2_PIN			0

// UART Peripheral
#define HW_UART_DEV				SD3
#define HW_UART_GPIO_AF			GPIO_AF_USART3
#define HW_UART_TX_PORT			GPIOB
#define HW_UART_TX_PIN			10
#define HW_UART_RX_PORT			GPIOD
#define HW_UART_RX_PIN			9          //Freeing B11 for fault LED. PD9 not available in LQFT64 package

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

// Resolver interface pins
#define AD2S1205_SAMPLE_GPIO	GPIOB
#define AD2S1205_SAMPLE_PIN		3
#define AD2S1205_RDVEL_GPIO     GPIOC
#define AD2S1205_RDVEL_PIN      14

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
#define HW_SPI_PIN_NSS			15
#define HW_SPI_PORT_SCK			GPIOC
#define HW_SPI_PIN_SCK			10
#define HW_SPI_PORT_MOSI		GPIOC
#define HW_SPI_PIN_MOSI			12
#define HW_SPI_PORT_MISO		GPIOC
#define HW_SPI_PIN_MISO			11

// Measurement macros
#define ADC_V_L1				(ADC_Value[ADC_IND_SENS1]-2048)		//phase voltages are centered in 1.65V
#define ADC_V_L2				(ADC_Value[ADC_IND_SENS2]-2048)
#define ADC_V_L3				(ADC_Value[ADC_IND_SENS3]-2048)
#define ADC_V_ZERO				0

// Macros
#define READ_HALL1()			palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()			palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()			palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)

// Override dead time. See the stm32f4 reference manual for calculating this value.
#define HW_DEAD_TIME_NSEC					1400.0
#define HW_GATE_DRIVER_SUPPLY_MAX_VOLTAGE	16.0
#define HW_GATE_DRIVER_SUPPLY_MIN_VOLTAGE	14.0

// Default setting overrides
#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC
#endif
#ifndef MCCONF_FOC_SAMPLE_V0_V7
#define MCCONF_FOC_SAMPLE_V0_V7			true	// Run control loop in both v0 and v7 (requires phase shunts)
#endif

#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE			0.0		// For safety the board will be held in fault until mc_conf is configured by the user
#endif

// Execute FOC loop once every "FOC_CONTROL_LOOP_FREQ_DIVIDER" ADC ISR calls
#define FOC_CONTROL_LOOP_FREQ_DIVIDER	1

// Setting limits
#define HW_LIM_CURRENT					-600.0, 600.0
#define HW_LIM_CURRENT_IN				-500.0, 500.0
#define HW_LIM_CURRENT_ABS				0.0, 800.0
#define HW_LIM_VIN						0.0, 525.0
#define HW_LIM_ERPM						-100e3, 100e3
#define HW_LIM_DUTY_MIN					0.0, 0.1
#define HW_LIM_DUTY_MAX					0.0, 1.0
#define HW_LIM_TEMP_FET					-40.0, 110.0
#define HW_LIM_FOC_CTRL_LOOP_FREQ		5000.0, 24000.0	//at around 38kHz the RTOS starts crashing (26us FOC ISR)

// HW-specific functions
char hw_axiom_configure_FPGA(void);
void hw_axiom_DAC1_setdata(uint16_t data);
void hw_axiom_DAC2_setdata(uint16_t data);
float hw_axiom_get_current_sensor_gain(void);
float hw_axiom_get_highest_IGBT_temp(void);
float hw_axiom_read_input_current(void);
void hw_axiom_get_input_current_offset(void);
void hw_axiom_start_input_current_sensor_offset_measurement(void);
float hw_axiom_temp_sensor_filter(uint8_t);
float hw_axiom_NTC_res_motor_filter(uint16_t);

#endif /* HW_AXIOM_H_ */
