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

#ifndef CMND_HALL_ANALYZE_H_
#define CMND_HALL_ANALYZE_H_

#include "terminal.h"
#include "commands.h"

// Functions
static void cmnd_hall_analyze(float current)
{

   if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max) {
      commands_printf("Starting hall sensor analysis...\n");

      mc_interface_lock();
      mc_configuration *mcconf = mempools_alloc_mcconf();
      *mcconf = *mc_interface_get_configuration();
      mc_motor_type motor_type_old = mcconf->motor_type;
      mcconf->motor_type = MOTOR_TYPE_FOC;
      mc_interface_set_configuration(mcconf);

      commands_init_plot("Angle", "Hall Sensor State");
      commands_plot_add_graph("Hall 1");
      commands_plot_add_graph("Hall 2");
      commands_plot_add_graph("Hall 3");
      commands_plot_add_graph("Combined");

      float phase = 0.0;

      for (int i = 0;i < 1000;i++) {
         timeout_reset();
         mcpwm_foc_set_openloop_phase((float)i * current / 1000.0, phase);
         chThdSleepMilliseconds(1);
      }

      bool is_second_motor = mc_interface_get_motor_thread() == 2;
      int hall_last = utils_read_hall(is_second_motor, mcconf->m_hall_extra_samples);
      float transitions[7] = {0.0};
      int states[8] = {-1, -1, -1, -1, -1, -1, -1, -1};
      int transition_index = 0;

      for (int i = 0;i < 720;i++) {
         int hall = utils_read_hall(is_second_motor, mcconf->m_hall_extra_samples);
         if (hall_last != hall) {
            if (transition_index < 7) {
               transitions[transition_index++] = phase;
            }

            for (int j = 0;j < 8;j++) {
               if (states[j] == hall || states[j] == -1) {
                  states[j] = hall;
                  break;
               }
            }
         }
         hall_last = hall;

         // Notice that the plots are offset slightly in Y, to make it easier to see them.
         commands_plot_set_graph(0);
         commands_send_plot_points(phase, (float)(hall & 1) * 1.02);
         commands_plot_set_graph(1);
         commands_send_plot_points(phase, (float)((hall >> 1) & 1) * 1.04);
         commands_plot_set_graph(2);
         commands_send_plot_points(phase, (float)((hall >> 2) & 1) * 1.06);
         commands_plot_set_graph(3);
         commands_send_plot_points(phase, (float)hall);

         phase += 1.0;
         timeout_reset();
         mcpwm_foc_set_openloop_phase(current, phase);
         chThdSleepMilliseconds(20);
      }

      mc_interface_lock_override_once();
      mc_interface_release_motor();
      mcconf->motor_type = motor_type_old;
      mc_interface_set_configuration(mcconf);
      mempools_free_mcconf(mcconf);
      mc_interface_unlock();

      int state_num = 0;
      for (int i = 0;i < 8;i++) {
         if (states[i] != -1) {
            state_num++;
         }
      }

      if (state_num == 6) {
         commands_printf("Found 6 different states. This seems correct.\n");
      } else {
         commands_printf("Found %d different states. Something is most likely wrong...\n", state_num);
      }

      float min = 900.0;
      float max = 0.0;
      for (int i = 0;i < 6;i++) {
         float diff = fabsf(utils_angle_difference(transitions[i], transitions[i + 1]));
         commands_printf("Hall diff %d: %.1f degrees", i + 1, (double)diff);
         if (diff < min) {
            min = diff;
         }
         if (diff > max) {
            max = diff;
         }
      }

      float deviation = (max - min) / 2.0;
      if (deviation < 5) {
         commands_printf("Maximum deviation: %.2f degrees. This is good alignment.\n", (double)deviation);
      } else if ((max - min) < 10) {
         commands_printf("Maximum deviation: %.2f degrees. This is OK, but not great alignment.\n", (double)deviation);
      } else if ((max - min) < 15) {
         commands_printf("Maximum deviation: %.2f degrees. This is bad, but probably usable alignment.\n", (double)deviation);
      } else {
         commands_printf("Maximum deviation: %.2f degrees. The hall sensors are significantly misaligned. This has "
               "to be fixed for proper operation.\n", (double)(max - min));
      }

      commands_printf("Done. Go to the Realtime Data > Experiment page to see the plot.\n");
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}

#endif  // CMND_HALL_ANALYZE_H_
