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
#ifndef HW_STORMCORE_60D_H_
#define HW_STORMCORE_60D_H_

#define HW_NAME                 "STORMCORE_60D"
#include "drv8323s.h"

// HW properties
#define HW_HAS_DRV8323S // for idrive do 0x073b for reg 4 (LS) and 0x034b for reg 3 (HS)
#define HW_HAS_3_SHUNTS

#define DRV8323S_CUSTOM_SETTINGS(); drv8323s_set_current_amp_gain(CURRENT_AMP_GAIN); \
                                    drv8323s_write_reg(3,0x377); \
                                    drv8323s_write_reg(4,0x777);

//#define HW_DEAD_TIME_NSEC               360.0   // Dead time

#define HW_HAS_DUAL_MOTOR
//Switch Pins
#define HW_HAS_STORMCORE_SWITCH
#define HW_HAS_RGB_SWITCH
#define SWITCH_IN_GPIO       GPIOA
#define SWITCH_IN_PIN        15
#define SWITCH_OUT_GPIO       GPIOB
#define SWITCH_OUT_PIN        13
#define SWITCH_PRECHARGED_GPIO       GPIOE
#define SWITCH_PRECHARGED_PIN        2
#define SWITCH_LED_3_GPIO       GPIOD
#define SWITCH_LED_3_PIN        15
#define SWITCH_LED_2_GPIO       GPIOD
#define SWITCH_LED_2_PIN        10
#define SWITCH_LED_1_GPIO       GPIOD
#define SWITCH_LED_1_PIN        11


#define DCCAL_ON() //drv8323s_dccal_on()
#define DCCAL_OFF() //drv8323s_dccal_off()

#define HW_EARLY_INIT()      smart_switch_pin_init(); smart_switch_thread_start();



//Pins for BLE UART
//#define USE_ALT_UART_PORT

#define HW_UART_P_BAUD          115200
#define HW_UART_P_DEV           SD1
#define HW_UART_P_GPIO_AF       GPIO_AF_USART1
#define HW_UART_P_TX_PORT       GPIOA
#define HW_UART_P_TX_PIN        9
#define HW_UART_P_RX_PORT       GPIOA
#define HW_UART_P_RX_PIN        10

// SPI for DRV8301
#define DRV8323S_MOSI_GPIO       GPIOC
#define DRV8323S_MOSI_PIN        12
#define DRV8323S_MISO_GPIO       GPIOC
#define DRV8323S_MISO_PIN        11
#define DRV8323S_SCK_GPIO        GPIOC
#define DRV8323S_SCK_PIN         10
#define DRV8323S_CS_GPIO         GPIOC
#define DRV8323S_CS_PIN          13
#define DRV8323S_CS_GPIO2        GPIOD
#define DRV8323S_CS_PIN2         2
#define DRV8323S_CS_GPIO3        GPIOE
#define DRV8323S_CS_PIN3         15

// Macros
#define ENABLE_GATE()           palSetPad(GPIOE, 14);
#define DISABLE_GATE()          palClearPad(GPIOE, 14)
#define ENABLE_GATE2()          palSetPad(GPIOD, 4);
#define DISABLE_GATE2()         palClearPad(GPIOD, 4)

#define ADC_SW_EN_PORT         GPIOB
#define ADC_SW_EN_PIN          12
#define ADC_SW_1_PORT         GPIOD
#define ADC_SW_1_PIN          7
#define ADC_SW_2_PORT         GPIOB
#define ADC_SW_2_PIN          3
#define ADC_SW_3_PORT         GPIOE
#define ADC_SW_3_PIN          7

#define ENABLE_MOS_TEMP1()           palClearPad(ADC_SW_EN_PORT, ADC_SW_EN_PIN); palClearPad(ADC_SW_1_PORT, ADC_SW_1_PIN );\
    palClearPad(ADC_SW_2_PORT, ADC_SW_2_PIN ); palClearPad(ADC_SW_3_PORT, ADC_SW_3_PIN );
#define ENABLE_MOS_TEMP2()           palClearPad(ADC_SW_EN_PORT, ADC_SW_EN_PIN); palSetPad(ADC_SW_1_PORT, ADC_SW_1_PIN );\
    palClearPad(ADC_SW_2_PORT, ADC_SW_2_PIN ); palClearPad(ADC_SW_3_PORT, ADC_SW_3_PIN );
#define ENABLE_MOT_TEMP1()           palClearPad(ADC_SW_EN_PORT, ADC_SW_EN_PIN); palClearPad(ADC_SW_1_PORT, ADC_SW_1_PIN );\
    palSetPad(ADC_SW_2_PORT, ADC_SW_2_PIN ); palClearPad(ADC_SW_3_PORT, ADC_SW_3_PIN );
#define ENABLE_MOT_TEMP2()           palClearPad(ADC_SW_EN_PORT, ADC_SW_EN_PIN); palSetPad(ADC_SW_1_PORT, ADC_SW_1_PIN );\
    palSetPad(ADC_SW_2_PORT, ADC_SW_2_PIN ); palClearPad(ADC_SW_3_PORT, ADC_SW_3_PIN );
#define ENABLE_V_BATT_DIV()           palClearPad(ADC_SW_EN_PORT, ADC_SW_EN_PIN); palSetPad(ADC_SW_1_PORT, ADC_SW_1_PIN );\
    palSetPad(ADC_SW_2_PORT, ADC_SW_2_PIN ); palSetPad(ADC_SW_3_PORT, ADC_SW_3_PIN );


#define IS_DRV_FAULT()          (!palReadPad(GPIOE, 3))
#define IS_DRV_FAULT2()          (!palReadPad(GPIOD, 3))

#define LED_GREEN_ON()          palSetPad(GPIOC, 9); //palClearPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN);
#define LED_GREEN_OFF()         palClearPad(GPIOC, 9); //palSetPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN);
#define LED_RED_ON()            palSetPad(GPIOA, 8); //palClearPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN);
#define LED_RED_OFF()           palClearPad(GPIOA, 8); //palSetPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN);
#define LED_SWITCH_R_ON()            palClearPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN)
#define LED_SWITCH_R_OFF()           palSetPad(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN)
#define LED_SWITCH_G_ON()            palClearPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)
#define LED_SWITCH_G_OFF()           palSetPad(SWITCH_LED_2_GPIO, SWITCH_LED_2_PIN)
#define LED_SWITCH_B_ON()            palClearPad(SWITCH_LED_1_GPIO, SWITCH_LED_1_PIN)
#define LED_SWITCH_B_OFF()           palSetPad(SWITCH_LED_1_GPIO, SWITCH_LED_1_PIN)


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

#define HW_ADC_CHANNELS         15
#define HW_ADC_INJ_CHANNELS     2
#define HW_ADC_NBR_CONV         5

// ADC Indexes

#define ADC_IND_SENS2           0
#define ADC_IND_SENS3           1
#define ADC_IND_SENS1           2

#define ADC_IND_CURR1           3
#define ADC_IND_CURR2           4
#define ADC_IND_VIN_SENS        5

#define ADC_IND_CURR3           6
#define ADC_IND_CURR4           7
#define ADC_IND_VM_SENSE        8

#define ADC_IND_CURR6           9
#define ADC_IND_CURR5           10
#define ADC_IND_SENS4           11

#define ADC_IND_ADC_MUX         12
#define ADC_IND_SENS5           13
#define ADC_IND_SENS6           14











// ADC macros and settings

// Component parameters (can be overridden)
#ifndef V_REG
#define V_REG                   3.3
#endif
#ifndef VIN_R1
#define VIN_R1                  68000.0
#endif
#ifndef VIN_R2
#define VIN_R2                  2200.0
#endif
#ifndef CURRENT_AMP_GAIN
#define CURRENT_AMP_GAIN        10.0
#endif
#ifndef CURRENT_SHUNT_RES
#define CURRENT_SHUNT_RES       0.001
#endif

// Input voltage
#define GET_INPUT_VOLTAGE()     ((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))
#define GET_BATT_VOLTAGE()      ((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_ADC_MUX] * ((VIN_R1 + VIN_R2) / VIN_R2))
#define GET_VM_SENSE_VOLTAGE()  ((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VM_SENSE] * ((VIN_R1 + VIN_R2) / VIN_R2))

// Voltage on ADC channel
#define ADC_VOLTS(ch)           ((float)ADC_Value[ch] / 4095.0 * V_REG)

#define SHUTDOWN_RESET()        mc_interface_reset_seconds_inactive()

// NTC Termistors
#define NTC_RES(adc_val)        (10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side // High side ->((4095.0 * 10000.0) / adc_val - 10000.0)
#define NTC_TEMP(adc_ind)       (1.0 / ((logf(NTC_RES(ADC_Value[adc_ind]) / 10000.0) / 3434.0) + (1.0 / 298.15)) - 273.15)

#define NTC_RES_MOTOR(adc_val)  (10000.0 / ((4095.0 / (float)adc_val) - 1.0)) // Motor temp sensor on low side
#define NTC_TEMP_MOTOR(beta)    (1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_ADC_MUX]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)
#define NTC_TEMP_MOTOR2(beta)    (1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_ADC_MUX]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15)
// Double samples in beginning and end for positive current measurement.
// Useful when the shunt sense traces have noise that causes offset.
#ifndef CURR1_DOUBLE_SAMPLE
#define CURR1_DOUBLE_SAMPLE     0
#endif
#ifndef CURR2_DOUBLE_SAMPLE
#define CURR2_DOUBLE_SAMPLE     0
#endif

// Number of servo outputs
#define HW_SERVO_NUM            2

// UART Peripheral
#define HW_UART_DEV             SD3
#define HW_UART_GPIO_AF         GPIO_AF_USART3
#define HW_UART_TX_PORT         GPIOB
#define HW_UART_TX_PIN          10
#define HW_UART_RX_PORT         GPIOB
#define HW_UART_RX_PIN          11

// ICU Peripheral for servo decoding
#define HW_ICU_TIMER            TIM9
#define HW_ICU_DEV              ICUD9
#define HW_ICU_CHANNEL          ICU_CHANNEL_1
#define HW_ICU_GPIO_AF          GPIO_AF_TIM9
#define HW_ICU_GPIO             GPIOE
#define HW_ICU_PIN              5

// I2C Peripheral
#define HW_I2C_DEV              I2CD2
#define HW_I2C_GPIO_AF          GPIO_AF_I2C2
#define HW_I2C_SCL_PORT         GPIOB
#define HW_I2C_SCL_PIN          10
#define HW_I2C_SDA_PORT         GPIOB
#define HW_I2C_SDA_PIN          11

// Hall/encoder pins
#define HW_HALL_ENC_GPIO1       GPIOD
#define HW_HALL_ENC_PIN1        13
#define HW_HALL_ENC_GPIO2       GPIOD
#define HW_HALL_ENC_PIN2        12
#define HW_HALL_ENC_GPIO3       GPIOD
#define HW_HALL_ENC_PIN3        14
#define HW_ENC_TIM              TIM4
#define HW_ENC_TIM_AF           GPIO_AF_TIM4
#define HW_ENC_TIM_CLK_EN()     RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE)
#define HW_ENC_EXTI_PORTSRC     EXTI_PortSourceGPIOD
#define HW_ENC_EXTI_PINSRC      EXTI_PinSource14
#define HW_ENC_EXTI_CH          EXTI15_10_IRQn
#define HW_ENC_EXTI_LINE        EXTI_Line14
#define HW_ENC_EXTI_ISR_VEC     EXTI15_10_IRQHandler
#define HW_ENC_TIM_ISR_CH       TIM4_IRQn
#define HW_ENC_TIM_ISR_VEC      TIM4_IRQHandler

#define HW_HALL_ENC_GPIO4       GPIOB
#define HW_HALL_ENC_PIN4        4
#define HW_HALL_ENC_GPIO5       GPIOB
#define HW_HALL_ENC_PIN5        6
#define HW_HALL_ENC_GPIO6       GPIOB
#define HW_HALL_ENC_PIN6        7
#define HW_ENC_TIM2              TIM3
#define HW_ENC_TIM_AF2           GPIO_AF_TIM3
#define HW_ENC_TIM_CLK_EN2()     RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE)
#define HW_ENC_EXTI_PORTSRC2     EXTI_PortSourceGPIOB
#define HW_ENC_EXTI_PINSRC2      EXTI_PinSource7
#define HW_ENC_EXTI_CH2          EXTI9_5_IRQn
#define HW_ENC_EXTI_LINE2        EXTI_Line6
#define HW_ENC_EXTI_ISR_VEC2     EXTI9_5_IRQHandler
#define HW_ENC_TIM_ISR_CH2       TIM3_IRQn
#define HW_ENC_TIM_ISR_VEC2      TIM3_IRQHandler

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
#define NRF5x_SWDIO_PIN         6
#define NRF5x_SWCLK_GPIO        GPIOD
#define NRF5x_SWCLK_PIN         5

#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE       MOTOR_TYPE_FOC
#endif

// SPI pins
#define HW_SPI_DEV              SPID1
#define HW_SPI_GPIO_AF          GPIO_AF_SPI1
#define HW_SPI_PORT_NSS         GPIOB
#define HW_SPI_PIN_NSS          11
#define HW_SPI_PORT_SCK         GPIOA
#define HW_SPI_PIN_SCK          5
#define HW_SPI_PORT_MOSI        GPIOB
#define HW_SPI_PIN_MOSI         5
#define HW_SPI_PORT_MISO        GPIOA
#define HW_SPI_PIN_MISO         6

// Measurement macros
#define ADC_V_L1                ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2                ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3                ADC_Value[ADC_IND_SENS3]
#define ADC_V_L4                ADC_Value[ADC_IND_SENS4]
#define ADC_V_L5                ADC_Value[ADC_IND_SENS5]
#define ADC_V_L6                ADC_Value[ADC_IND_SENS6]
#define ADC_V_ZERO              (ADC_Value[ADC_IND_VIN_SENS] / 2)

// Macros
#define READ_HALL1()            palReadPad(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1)
#define READ_HALL2()            palReadPad(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2)
#define READ_HALL3()            palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)
#define READ_HALL4()            palReadPad(HW_HALL_ENC_GPIO4, HW_HALL_ENC_PIN4)
#define READ_HALL5()            palReadPad(HW_HALL_ENC_GPIO5, HW_HALL_ENC_PIN5)
#define READ_HALL6()            palReadPad(HW_HALL_ENC_GPIO6, HW_HALL_ENC_PIN6)

//CAN
#define HW_CANH_PORT            GPIOD
#define HW_CANH_PIN             0
#define HW_CANL_PORT            GPIOD
#define HW_CANL_PIN             1
#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE            58.0
#endif
// Setting limits
#define HW_LIM_CURRENT          -150.0, 150.0
#define HW_LIM_CURRENT_IN       -120.0, 120.0
#define HW_LIM_CURRENT_ABS      0.0, 200.0
#define HW_LIM_VIN              6.0, 58.0
#define HW_LIM_ERPM             -200e3, 200e3
#define HW_LIM_DUTY_MIN         0.0, 0.1
#define HW_LIM_DUTY_MAX         0.0, 0.95
#define HW_LIM_TEMP_FET         -40.0, 120.0

#endif /* HW_STORMCORE_60D_H_ */
