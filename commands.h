/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#ifndef COMMANDS_H_
#define COMMANDS_H_

#include "datatypes.h"

// Functions
void commands_init(void);
void commands_set_send_func(void(*func)(unsigned char *data, unsigned int len));
void commands_send_packet(unsigned char *data, unsigned int len);
void commands_process_packet(unsigned char *data, unsigned int len);
void commands_printf(const char* format, ...);
void commands_send_rotor_pos(float rotor_pos);
void commands_send_experiment_samples(float *samples, int len);
disp_pos_mode commands_get_disp_pos_mode(void);
void commands_set_app_data_handler(void(*func)(unsigned char *data, unsigned int len));
void commands_send_app_data(unsigned char *data, unsigned int len);
void commands_send_appconf(COMM_PACKET_ID packet_id, app_configuration *appconf);

#endif /* COMMANDS_H_ */
