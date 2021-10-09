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

#ifndef CMND_GPIO_CTRL_H_
#define CMND_GPIO_CTRL_H_

#include "terminal.h"
#include "commands.h"
#include "mc_interface.h"
#include "mcpwm.h"

// Functions
static void cmnd_processs_io_board_set_output_pwm(int id, int channel, float duty)
{
   if (id >= 0 && channel >= 0 && duty >= 0.0 && duty <= 1.0) {
      comm_can_io_board_set_output_pwm(id, channel, duty);
      commands_printf("OK\n");
   } else {
      commands_printf("Invalid arguments\n");
   }
}


static void cmnd_processs_io_board_set_output(int id, int channel, int state)
{
   if (id >= 0 && channel >= 0 && state >= 0) {
      comm_can_io_board_set_output_digital(id, channel, state);
      commands_printf("OK\n");
   } else {
      commands_printf("Invalid arguments\n");
   }
}


#endif  // CMND_GPIO_CTRL_H_
