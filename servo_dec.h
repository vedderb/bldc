/*
 * servo_dec.h
 *
 *  Created on: 20 jan 2013
 *      Author: benjamin
 */

#ifndef SERVO_DEC_H_
#define SERVO_DEC_H_

#include <stdint.h>

// Servo function indexes
#define SERVODEC_IND_STEERING		0
#define SERVODEC_IND_THROTTLE		1
#define SERVODEC_IND_AUX			2

// Functions
void servodec_init(void);
void servodec_timerfunc(void);
void servodec_int_handler(void);
int8_t servodec_get_servo(int servo_num);
uint32_t servodec_get_time_since_update(void);

#endif /* SERVO_DEC_H_ */
