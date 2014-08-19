/*
	Copyright 2014 Cyril Holweck	cyril.holweck@free.fr

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

/*
 * hw_pl.h
 *
 *  Created on: 04 jul 2014
 *      Author: cyrilh
 */

#ifndef HW_PL_H_
#define HW_PL_H_

// Macros
#define ENABLE_GATE()			palSetPad(GPIOC, 13)
#define DISABLE_GATE()			palClearPad(GPIOC, 13)
#define DCCAL_ON()				palSetPad(GPIOB, 12)
#define DCCAL_OFF()				palClearPad(GPIOB, 12)
#define IS_DRV_FAULT()			(!palReadPad(GPIOB, 8))

#define LED1_ON()			palSetPad(GPIOB, 4)
#define LED1_OFF()			palClearPad(GPIOB, 4)
#define LED2_ON()			palSetPad(GPIOB, 3)
#define LED2_OFF()			palClearPad(GPIOB, 3)

/** TODO: Add second gate */

/*
 * ADC Vector (from hw_pl.c)
 *
 * 0:	IN10	M2_SENS1
 * 1:	IN11	M2_SENS2
 * 2:	IN12	M2_SENS3
 * 3:	IN0		M1_SENS1
 * 4:	IN1		M1_SENS2
 * 5:	IN2		M1_SENS3
 * 6:	IN14	M2_CURR
 * 7:	IN16	TEMP_PCB (STM32 internal temperature sensor)
 * 8:	IN15	M2_SHUNT2
 * 9:	IN6		AN_IN (HV_SENS)
 * 10:	IN3		M1_CURR
 * 11:	IN13	ADC_EXT
 
 * RFU:	IN4		RFU (M1_SHUNT2)
 */

#define HW_ADC_CHANNELS				12
#define HW_ADC_NBR_CONV				4

// ADC Indexes
#define ADC_IND_M2_SENS1	0
#define ADC_IND_M2_SENS2	1
#define ADC_IND_M2_SENS3	2
#define ADC_IND_M2_CURR		6
#define ADC_IND_VIN_SENS	9
#define ADC_IND_EXT			11
#define ADC_IND_TEMP_PCB	7

#define ADC_IND_SENS1		ADC_IND_M2_SENS1
#define ADC_IND_SENS2		ADC_IND_M2_SENS2
#define ADC_IND_SENS3		ADC_IND_M2_SENS3
#define ADC_IND_CURR1		ADC_IND_M2_CURR
#define ADC_IND_CURR2		ADC_IND_M2_CURR

// Voltage on ADC channel
#define ADC_VOLTS(ch)			((float)ADC_Value[ch] / 4096.0 * V_REG)
#define ADC_FROMVOLTS(volts)	((float)volts / V_REG * 4096.0)

// Measurement macros
/* If we PWM the upper switch and drive permanently the lower switch,
 * if sample during PWM off time when upper phase is freewheeling,
 * zero crossing is true phase 0V crossing.
 * ADC_V_ZERO is a constant value depending on voltage divider / pull resistor
*/
#define SENS_OFFSET				(3.3*33.0/(220.0+33.0))
#define OPAMP_GAIN				((220.0+100.0)/100.0)
#define ADC_V_L1				ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2				ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3				ADC_Value[ADC_IND_SENS3]
#define ADC_V_ZERO				ADC_FROMVOLTS(SENS_OFFSET*OPAMP_GAIN)

// Macros (hall sensors)
#define READ_HALL1()			0
#define READ_HALL2()			0
#define READ_HALL3()			0

// ADC macros and settings

// Component parameters (CPU power supply, voltage divider for input high voltage
#define V_REG				3.3
#define VIN_R1				220000.0
#define VIN_R2				15000.0
#define CURRENT_AMP_GAIN	10.0
#define CURRENT_SHUNT_RES	0.001

// Input voltage
#define GET_INPUT_VOLTAGE()	( ADC_VOLTS(ADC_IND_VIN_SENS) * ((VIN_R1 + VIN_R2) / VIN_R2))

// NTC Termistors
/** TODO: figure out how to use internal temperature sensor */
#define TS_CAL1     (*((uint16_t*) 0x1FFF7A2C))
#define TS_CAL2     (*((uint16_t*) 0x1FFF7A2E))
#define NTC_RES(adc_val)	(0.0)
#define NTC_TEMP(adc_ind)	( 30.0 + (80.0*(ADC_Value[ADC_IND_TEMP_PCB]-TS_CAL1))/(TS_CAL2-TS_CAL1) )


// Number of servo outputs
/** TODO: fix that */
#define HW_SERVO_NUM		2

#endif /* HW_R2_H_ */
