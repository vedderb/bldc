/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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
 * main.h
 *
 *  Created on: 10 jul 2012
 *      Author: BenjaminVe
 */

#ifndef MAIN_H_
#define MAIN_H_

#include <stdint.h>

// Component parameters
#define V_REG		3.3
#define VIN_R1		33000.0
#define VIN_R2		2200.0

// Input voltage
#define GET_INPUT_VOLTAGE()	((V_REG / 4095.0) * (float)ADC_Value[ADC_IND_VIN_SENS] * ((VIN_R1 + VIN_R2) / VIN_R2))

// Voltage on ADC channel
#define ADC_VOLTS(ch)		((float)ADC_Value[ch] / 4096.0 * 3.3)

// Sharp sensors

// EXternal Variables
extern volatile uint16_t ADC_Value[];

/*
 * ADC Vector
 *
 * 0:	IN0		SENS3
 * 1:	IN1		SENS2
 * 2:	IN2		SENS1
 * 3:	IN5		CURR1
 * 4:	IN6		CURR2
 * 5:	IN3		NC
 * 6:	IN10	TEMP_MOTOR
 * 7:	IN11	NC
 * 8:	IN12	AN_IN
 * 9:	IN13	NC
 * 10:	IN15	ADC_EXT
 * 11:	IN3		NC
 */

// ADC Indexes
#define ADC_IND_SENS1		2
#define ADC_IND_SENS2		1
#define ADC_IND_SENS3		0
#define ADC_IND_CURR1		3
#define ADC_IND_CURR2		4
#define ADC_IND_VIN_SENS	8
#define ADC_IND_EXT			10

// Measurement macros
#define ADC_V_L1					ADC_Value[ADC_IND_SENS1]
#define ADC_V_L2					ADC_Value[ADC_IND_SENS2]
#define ADC_V_L3					ADC_Value[ADC_IND_SENS3]

// Function prototypes
void main_dma_adc_handler(void);
float main_get_last_adc_isr_duration(void);
void main_process_packet(unsigned char *data, unsigned char len);

#endif /* MAIN_H_ */
