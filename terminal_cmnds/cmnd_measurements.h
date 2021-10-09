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

#ifndef CMND_MEASUREMENTS_H_
#define CMND_MEASUREMENTS_H_

#include "terminal.h"
#include "commands.h"
#include "mc_interface.h"
#include "mcpwm.h"
#include "mempools.h"

// Functions

static void cmnd_processs_param_detect(float current, float min_rpm, float low_duty)
{
   if (current > 0.0 && current < mc_interface_get_configuration()->l_current_max &&
         min_rpm > 10.0 && min_rpm < 3000.0 &&
         low_duty > 0.02 && low_duty < 0.8) {

      float cycle_integrator;
      float coupling_k;
      int8_t hall_table[8];
      int hall_res;
      if (conf_general_detect_motor_param(current, min_rpm, low_duty, &cycle_integrator, &coupling_k, hall_table, &hall_res)) {
         commands_printf("Cycle integrator limit: %.2f", (double)cycle_integrator);
         commands_printf("Coupling factor: %.2f", (double)coupling_k);

         if (hall_res == 0) {
            commands_printf("Detected hall sensor table:");
            commands_printf("%i, %i, %i, %i, %i, %i, %i, %i\n",
                  hall_table[0], hall_table[1], hall_table[2], hall_table[3],
                  hall_table[4], hall_table[5], hall_table[6], hall_table[7]);
         } else if (hall_res == -1) {
            commands_printf("Hall sensor detection failed:");
            commands_printf("%i, %i, %i, %i, %i, %i, %i, %i\n",
                  hall_table[0], hall_table[1], hall_table[2], hall_table[3],
                  hall_table[4], hall_table[5], hall_table[6], hall_table[7]);
         } else if (hall_res == -2) {
            commands_printf("WS2811 enabled. Hall sensors cannot be used.\n");
         } else if (hall_res == -3) {
            commands_printf("Encoder enabled. Hall sensors cannot be used.\n");
         }
      } else {
         commands_printf("Detection failed. Try again with different parameters.\n");
      }
   } else {
      commands_printf("Invalid argument(s).\n");
   }

}
static void cmnd_processs_measure_res(float current)
{
   mc_configuration *mcconf = mempools_alloc_mcconf();
   *mcconf = *mc_interface_get_configuration();
   mc_configuration *mcconf_old = mempools_alloc_mcconf();
   *mcconf_old = *mc_interface_get_configuration();

   if (current > 0.0 && current <= mcconf->l_current_max) {
      mcconf->motor_type = MOTOR_TYPE_FOC;
      mc_interface_set_configuration(mcconf);

      commands_printf("Resistance: %.6f ohm\n", (double)mcpwm_foc_measure_resistance(current, 2000, true));

      mc_interface_set_configuration(mcconf_old);
   } else {
      commands_printf("Invalid argument(s).\n");
   }

   mempools_free_mcconf(mcconf);
   mempools_free_mcconf(mcconf_old);
}

static void cmnd_processs_measure_ind(float duty)
{
   if (duty > 0.0 && duty < 0.9) {
      mc_configuration *mcconf = mempools_alloc_mcconf();
      *mcconf = *mc_interface_get_configuration();
      mc_configuration *mcconf_old = mempools_alloc_mcconf();
      *mcconf_old = *mc_interface_get_configuration();

      mcconf->motor_type = MOTOR_TYPE_FOC;
      mc_interface_set_configuration(mcconf);

      float curr, ld_lq_diff;
      float ind = mcpwm_foc_measure_inductance(duty, 400, &curr, &ld_lq_diff);
      commands_printf("Inductance: %.2f uH, ld_lq_diff: %.2f uH (%.2f A)\n",
            (double)ind, (double)ld_lq_diff, (double)curr);

      mc_interface_set_configuration(mcconf_old);

      mempools_free_mcconf(mcconf);
      mempools_free_mcconf(mcconf_old);
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}


static void cmnd_processs_measure_linkage(float current, float duty, float min_erpm, float res)
{
   if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max &&
         min_erpm > 0.0 && duty > 0.02 && res >= 0.0) {
      float linkage;
      conf_general_measure_flux_linkage(current, duty, min_erpm, res, &linkage);
      commands_printf("Flux linkage: %.7f\n", (double)linkage);
   } else {
      commands_printf("Invalid argument(s).\n");
   }

}

static void cmnd_processs_measure_linkage_foc(float duty)
{
   if (duty > 0.0) {
      mc_configuration *mcconf = mempools_alloc_mcconf();
      *mcconf = *mc_interface_get_configuration();
      mc_configuration *mcconf_old = mempools_alloc_mcconf();
      *mcconf_old = *mc_interface_get_configuration();

      mcconf->motor_type = MOTOR_TYPE_FOC;
      mc_interface_set_configuration(mcconf);
      const float res = (3.0 / 2.0) * mcconf->foc_motor_r;

      // Disable timeout
      systime_t tout = timeout_get_timeout_msec();
      float tout_c = timeout_get_brake_current();
      timeout_reset();
      timeout_configure(60000, 0.0);

      for (int i = 0;i < 100;i++) {
         mc_interface_set_duty(((float)i / 100.0) * duty);
         chThdSleepMilliseconds(20);
      }

      float vq_avg = 0.0;
      float rpm_avg = 0.0;
      float samples = 0.0;
      float iq_avg = 0.0;
      for (int i = 0;i < 1000;i++) {
         vq_avg += mcpwm_foc_get_vq();
         rpm_avg += mc_interface_get_rpm();
         iq_avg += mc_interface_get_tot_current_directional();
         samples += 1.0;
         chThdSleepMilliseconds(1);
      }

      mc_interface_release_motor();
      mc_interface_set_configuration(mcconf_old);

      mempools_free_mcconf(mcconf);
      mempools_free_mcconf(mcconf_old);

      // Enable timeout
      timeout_configure(tout, tout_c);

      vq_avg /= samples;
      rpm_avg /= samples;
      iq_avg /= samples;

      float linkage = (vq_avg - res * iq_avg) / (rpm_avg * ((2.0 * M_PI) / 60.0));

      commands_printf("Flux linkage: %.7f\n", (double)linkage);
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}

static void cmnd_processs_measure_res_ind(void)
{
   mc_configuration *mcconf = mempools_alloc_mcconf();
   *mcconf = *mc_interface_get_configuration();
   mc_configuration *mcconf_old = mempools_alloc_mcconf();
   *mcconf_old = *mc_interface_get_configuration();

   mcconf->motor_type = MOTOR_TYPE_FOC;
   mc_interface_set_configuration(mcconf);

   float res = 0.0;
   float ind = 0.0;
   mcpwm_foc_measure_res_ind(&res, &ind);
   commands_printf("Resistance: %.6f ohm", (double)res);
   commands_printf("Inductance: %.2f microhenry\n", (double)ind);

   mc_interface_set_configuration(mcconf_old);

   mempools_free_mcconf(mcconf);
   mempools_free_mcconf(mcconf_old);
}

static void cmnd_processs_measure_linkage_openloop(float current, float duty, float erpm_per_sec, float res, float ind)
{

   if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max &&
         erpm_per_sec > 0.0 && duty > 0.02 && res >= 0.0 && ind >= 0.0) {
      float linkage, linkage_undriven, undriven_samples;
      commands_printf("Measuring flux linkage...");
      conf_general_measure_flux_linkage_openloop(current, duty, erpm_per_sec, res, ind,
            &linkage, &linkage_undriven, &undriven_samples);
      commands_printf(
            "Flux linkage            : %.7f\n"
            "Flux Linkage (undriven) : %.7f\n"
            "Undriven samples        : %.1f\n",
            (double)linkage, (double)linkage_undriven, (double)undriven_samples);
   } else {
      commands_printf("Invalid argument(s).\n");
   }
}

#endif  // CMND_MEASUREMENTS_H_
