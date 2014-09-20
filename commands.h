/*
 * commands.h
 *
 *  Created on: 19 sep 2014
 *      Author: benjamin
 */

#ifndef COMMANDS_H_
#define COMMANDS_H_

#include "datatypes.h"

// Functions
void commands_set_send_func(void(*func)(unsigned char *data, unsigned char len));
void commands_process_packet(unsigned char *data, unsigned char len);
void commands_printf(char* format, ...);
void commands_send_samples(uint8_t *data, int len);
void commands_send_rotor_pos(float rotor_pos);
void commands_print_fault_code(mc_fault_code fault_code);
void commands_send_experiment_samples(float *samples, int len);

#endif /* COMMANDS_H_ */
