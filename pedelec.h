/*
 * pedelec.h
 *
 *  Created on: Sep 3, 2019
 *      Author: motorcontrol
 */

#ifndef PEDELEC_H_
#define PEDELEC_H_

#include "stdbool.h"
#include "stdint.h"


bool pedelec_get_pulse_detected_flag(void);
void pedelec_set_pulse_detected_flag(bool value);
void pedelec_tim_isr(void);
float pedelec_get_frecuency(void);
float pedelec_get_rpm(float frecuency, uint8_t magnets);
void pedelec_periodic_task( uint32_t delta_ms );
void pedelec_init(void);

#endif /* PEDELEC_H_ */
