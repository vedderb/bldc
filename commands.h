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
 * commands.h
 *
 *  Created on: 19 sep 2014
 *      Author: benjamin
 */

#ifndef COMMANDS_H_
#define COMMANDS_H_

#include "datatypes.h"

// Functions
void commands_init(void);
void commands_set_send_func(void(*func)(unsigned char *data, unsigned int len));
void commands_send_packet(unsigned char *data, unsigned int len);
void commands_process_packet(unsigned char *data, unsigned int len);
void commands_printf(char* format, ...);
void commands_send_samples(uint8_t *data, int len);
void commands_send_rotor_pos(float rotor_pos);
void commands_send_experiment_samples(float *samples, int len);
disp_pos_mode commands_get_disp_pos_mode(void);
void commands_set_app_data_handler(void(*func)(unsigned char *data, unsigned int len));

#endif /* COMMANDS_H_ */
