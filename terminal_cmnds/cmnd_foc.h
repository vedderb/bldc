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

#ifndef CMND_FOC_H_
#define CMND_FOC_H_

#include "terminal.h"
#include "commands.h"
#include "mc_interface.h"
#include "mcpwm.h"

// Functions

static void cmnd_processs_foc_state(void)
{
   mcpwm_foc_print_state();
   commands_printf(" ");
}


static void cmnd_processs_foc_openloop(float current, float erpm)
{
   if (current >= 0.0 && erpm >= 0.0) {
      timeout_reset();
      mcpwm_foc_set_openloop(current, erpm);
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}


static void cmnd_processs_foc_openloop_duty(float duty, float erpm)
{
   if (duty >= 0.0 && erpm >= 0.0) {
      timeout_reset();
      mcpwm_foc_set_openloop_duty(duty, erpm);
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}


static void cmnd_processs_foc_sensors_detect_apply(float current)
{
   if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max) {
      int res = conf_general_autodetect_apply_sensors_foc(current, true, true);

      if (res == 0) {
         commands_printf("No sensors found, using sensorless mode.\n");
      } else if (res == 1) {
         commands_printf("Found hall sensors, using them.\n");
      } else if (res == 2) {
         commands_printf("Found AS5047 encoder, using it.\n");
      } else {
         commands_printf("Detection error: %d\n", res);
      }
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}


static void cmnd_processs_foc_detect_apply_all(float max_power_loss)
{

   if (max_power_loss > 0.0) {
      int motor_thread_old = mc_interface_get_motor_thread();

      commands_printf("Running detection...");
      int res = conf_general_detect_apply_all_foc(max_power_loss, true, true);

      commands_printf("Res: %d", res);
      mc_interface_select_motor_thread(1);

      if (res >= 0) {
         commands_printf("Detection finished and applied. Results:");
         const volatile mc_configuration *mcconf = mc_interface_get_configuration();
#ifdef HW_HAS_DUAL_MOTORS
         commands_printf("\nMOTOR 1\n");
#endif
         commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
         commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
         commands_printf("Motor L             : %.2f microH", (double)(mcconf->foc_motor_l * 1e6));
         commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
         commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
         if (mcconf->foc_temp_comp) {
            commands_printf("Temp Comp Base Temp : %.1f degC", (double)mcconf->foc_temp_comp_base_temp);
         }

         if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
            commands_printf("No sensors found, using sensorless mode.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
            commands_printf("Found hall sensors, using them.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
            commands_printf("Found AS5047 encoder, using it.\n");
         } else {
            commands_printf("Detection error: %d\n", res);
         }
#ifdef HW_HAS_DUAL_MOTORS
         mc_interface_select_motor_thread(2);
         mcconf = mc_interface_get_configuration();
         commands_printf("\nMOTOR 2\n");
         commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
         commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
         commands_printf("Motor L             : %.2f microH", (double)(mcconf->foc_motor_l * 1e6));
         commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
         commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
         if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
            commands_printf("No sensors found, using sensorless mode.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
            commands_printf("Found hall sensors, using them.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
            commands_printf("Found AS5047 encoder, using it.\n");
         } else {
            commands_printf("Detection error: %d\n", res);
         }
#endif
      } else {
         if (res == -10) {
            commands_printf("Could not measure flux linkage.");
         } else if (res == -11) {
            commands_printf("Fault code occurred during detection.");
         }

         commands_printf("Detection failed.\n");
      }

      mc_interface_select_motor_thread(motor_thread_old);
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}


static void cmnd_processs_foc_detect_apply_all_can(float max_power_loss)
{
   if (max_power_loss > 0.0) {
      commands_printf("Running detection...");
      int res = conf_general_detect_apply_all_foc_can(true, max_power_loss, 0.0, 0.0, 0.0, 0.0);

      commands_printf("Res: %d", res);

      if (res >= 0) {
         commands_printf("Detection finished and applied. Results:");
#ifdef HW_HAS_DUAL_MOTORS
         commands_printf("\nMOTOR 1\n");
#endif
         const volatile mc_configuration *mcconf = mc_interface_get_configuration();
         commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
         commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
         commands_printf("Motor L             : %.2f microH", (double)(mcconf->foc_motor_l * 1e6));
         commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
         commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
         if (mcconf->foc_temp_comp) {
            commands_printf("Temp Comp Base Temp : %.1f degC", (double)mcconf->foc_temp_comp_base_temp);
         }

         if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
            commands_printf("No sensors found, using sensorless mode.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
            commands_printf("Found hall sensors, using them.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
            commands_printf("Found AS5047 encoder, using it.\n");
         } else {
            commands_printf("Detection error: %d\n", res);
         }
#ifdef HW_HAS_DUAL_MOTORS
         mc_interface_select_motor_thread(2);
         mcconf = mc_interface_get_configuration();
         commands_printf("\nMOTOR 2\n");
         commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
         commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
         commands_printf("Motor L             : %.2f microH", (double)(mcconf->foc_motor_l * 1e6));
         commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
         commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
         if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
            commands_printf("No sensors found, using sensorless mode.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
            commands_printf("Found hall sensors, using them.\n");
         } else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
            commands_printf("Found AS5047 encoder, using it.\n");
         } else {
            commands_printf("Detection error: %d\n", res);
         }
         commands_printf("\nNote that this is only printing values of motors 1");
         commands_printf("and 2 of the currently connected unit, other motors");
         commands_printf("may have been detected, but won't be printed here");
#endif
      } else {
         if (res == -10) {
            commands_printf("Could not measure flux linkage.");
         } else if (res == -11) {
            commands_printf("Fault code occurred during detection.");
         }

         commands_printf("Detection failed.\n");
      }
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}


static void cmnd_processs_foc_encoder_detect(float current)
{
   mc_configuration *mcconf = mempools_alloc_mcconf();
   *mcconf = *mc_interface_get_configuration();

   if (current > 0.0 && current <= mcconf->l_current_max) {
      if (encoder_is_configured()) {
         mc_motor_type type_old = mcconf->motor_type;
         mcconf->motor_type = MOTOR_TYPE_FOC;
         mc_interface_set_configuration(mcconf);

         float offset = 0.0;
         float ratio = 0.0;
         bool inverted = false;
         mcpwm_foc_encoder_detect(current, true, &offset, &ratio, &inverted);

         mcconf->motor_type = type_old;
         mc_interface_set_configuration(mcconf);

         commands_printf("Offset   : %.2f", (double)offset);
         commands_printf("Ratio    : %.2f", (double)ratio);
         commands_printf("Inverted : %s\n", inverted ? "true" : "false");
      } else {
         commands_printf("Encoder not enabled.\n");
      }
   } else {
      commands_printf("Invalid argument(s).\n");
   }

   mempools_free_mcconf(mcconf);
}
#endif  // CMND_FOC_H_
