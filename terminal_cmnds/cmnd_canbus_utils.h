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

#ifndef CMND_CANBUS_UTILS_H_
#define CMND_CANBUS_UTILS_H_

#include "terminal.h"
#include "commands.h"

// Functions

static void cmnd_processs_can_devs(void)
{
   commands_printf("CAN devices seen on the bus the past second:\n");
   for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
      can_status_msg *msg = comm_can_get_status_msg_index(i);

      if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < 1.0) {
         commands_printf("ID                 : %i", msg->id);
         commands_printf("RX Time            : %i", msg->rx_time);
         commands_printf("Age (milliseconds) : %.2f", (double)(UTILS_AGE_S(msg->rx_time) * 1000.0));
         commands_printf("RPM                : %.2f", (double)msg->rpm);
         commands_printf("Current            : %.2f", (double)msg->current);
         commands_printf("Duty               : %.2f\n", (double)msg->duty);
      }

      io_board_adc_values *io_adc = comm_can_get_io_board_adc_1_4_index(i);
      if (io_adc->id >= 0 && UTILS_AGE_S(io_adc->rx_time) < 1.0) {
         commands_printf("IO Board ADC 1_4");
         commands_printf("ID                 : %i", io_adc->id);
         commands_printf("RX Time            : %i", io_adc->rx_time);
         commands_printf("Age (milliseconds) : %.2f", (double)(UTILS_AGE_S(io_adc->rx_time) * 1000.0));
         commands_printf("ADC                : %.2f %.2f %.2f %.2f\n",
               (double)io_adc->adc_voltages[0], (double)io_adc->adc_voltages[1],
               (double)io_adc->adc_voltages[2], (double)io_adc->adc_voltages[3]);
      }

      io_adc = comm_can_get_io_board_adc_5_8_index(i);
      if (io_adc->id >= 0 && UTILS_AGE_S(io_adc->rx_time) < 1.0) {
         commands_printf("IO Board ADC 5_8");
         commands_printf("ID                 : %i", io_adc->id);
         commands_printf("RX Time            : %i", io_adc->rx_time);
         commands_printf("Age (milliseconds) : %.2f", (double)(UTILS_AGE_S(io_adc->rx_time) * 1000.0));
         commands_printf("ADC                : %.2f %.2f %.2f %.2f\n",
               (double)io_adc->adc_voltages[0], (double)io_adc->adc_voltages[1],
               (double)io_adc->adc_voltages[2], (double)io_adc->adc_voltages[3]);
      }

      io_board_digial_inputs *io_in = comm_can_get_io_board_digital_in_index(i);
      if (io_in->id >= 0 && UTILS_AGE_S(io_in->rx_time) < 1.0) {
         commands_printf("IO Board Inputs");
         commands_printf("ID                 : %i", io_in->id);
         commands_printf("RX Time            : %i", io_in->rx_time);
         commands_printf("Age (milliseconds) : %.2f", (double)(UTILS_AGE_S(io_in->rx_time) * 1000.0));
         commands_printf("IN                 : %llu %llu %llu %llu %llu %llu %llu %llu\n",
               (io_in->inputs >> 0) & 1, (io_in->inputs >> 1) & 1,
               (io_in->inputs >> 2) & 1, (io_in->inputs >> 3) & 1,
               (io_in->inputs >> 4) & 1, (io_in->inputs >> 5) & 1,
               (io_in->inputs >> 6) & 1, (io_in->inputs >> 7) & 1);
      }
   }
}


static void cmnd_processs_can_scan(void)
{
   bool found = false;
   for (int i = 0;i < 254;i++) {
      HW_TYPE hw_type;
      if (comm_can_ping(i, &hw_type)) {
         commands_printf("Found %s with ID: %d", utils_hw_type_to_string(hw_type), i);
         found = true;
      }
   }

   if (found) {
      commands_printf("Done\n");
   } else {
      commands_printf("No CAN devices found\n");
   }
}
#endif  // CMND_CANBUS_UTILS_H_
