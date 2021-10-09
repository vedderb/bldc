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

#ifndef CMND_MISC_H_
#define CMND_MISC_H_

#include "terminal.h"
#include "commands.h"
#include "mc_interface.h"
#include "mcpwm.h"

#include "drv8301.h"
#include "drv8305.h"
#include "drv8320s.h"
#include "drv8323s.h"

// Functions
static void cmnd_processs_stop(void)
{
   mc_interface_set_duty(0);
   commands_printf("Motor stopped\n");
}

static void cmnd_processs_last_adc_duration(void)
{
   commands_printf("Latest ADC duration: %.4f ms", (double)(mcpwm_get_last_adc_isr_duration() * 1000.0));
   commands_printf("Latest injected ADC duration: %.4f ms", (double)(mc_interface_get_last_inj_adc_isr_duration() * 1000.0));
   commands_printf("Latest sample ADC duration: %.4f ms\n", (double)(mc_interface_get_last_sample_adc_isr_duration() * 1000.0));
}


static void cmnd_processs_kv(void)
{
   // Check if the motor is currently running in BLDC mode
   if (1) {
      if (1) {
         // Check if the motor is running
         commands_printf("Calculated KV: %.2f rpm/volt\n", (double)mcpwm_get_kv_filtered());
      } else {
         commands_printf("KV estimation is only available when the motor is running.");
      }
   } else {
      commands_printf("KV is only available when configured to operate BLDC mode.");
   }
}

static void cmnd_processs_faults(int fault_vec_write, fault_data *fault_vec)
{
   if (fault_vec_write == 0) {
      commands_printf("No faults registered since startup\n");
   } else {
      commands_printf("The following faults were registered since start:\n");
      for (int i = 0;i < fault_vec_write;i++) {
         commands_printf("Fault            : %s", mc_interface_fault_to_string(fault_vec[i].fault));
         commands_printf("Motor            : %d", fault_vec[i].motor);
         commands_printf("Current          : %.1f", (double)fault_vec[i].current);
         commands_printf("Current filtered : %.1f", (double)fault_vec[i].current_filtered);
         commands_printf("Voltage          : %.2f", (double)fault_vec[i].voltage);
   #ifdef HW_HAS_GATE_DRIVER_SUPPLY_MONITOR
         commands_printf("Gate drv voltage : %.2f", (double)fault_vec[i].gate_driver_voltage);
   #endif
         commands_printf("Duty             : %.3f", (double)fault_vec[i].duty);
         commands_printf("RPM              : %.1f", (double)fault_vec[i].rpm);
         commands_printf("Tacho            : %d", fault_vec[i].tacho);
         commands_printf("Cycles running   : %d", fault_vec[i].cycles_running);
         commands_printf("TIM duty         : %d", (int)((float)fault_vec[i].tim_top * fault_vec[i].duty));
         commands_printf("TIM val samp     : %d", fault_vec[i].tim_val_samp);
         commands_printf("TIM current samp : %d", fault_vec[i].tim_current_samp);
         commands_printf("TIM top          : %d", fault_vec[i].tim_top);
         commands_printf("Comm step        : %d", fault_vec[i].comm_step);
         commands_printf("Temperature      : %.2f", (double)fault_vec[i].temperature);
   #ifdef HW_HAS_DRV8301
         if (fault_vec[i].fault == FAULT_CODE_DRV) {
            commands_printf("DRV8301_FAULTS   : %s", drv8301_faults_to_string(fault_vec[i].drv8301_faults));
         }
   #elif defined(HW_HAS_DRV8320S)
         if (fault_vec[i].fault == FAULT_CODE_DRV) {
            commands_printf("DRV8320S_FAULTS  : %s", drv8320s_faults_to_string(fault_vec[i].drv8301_faults));
         }
   #elif defined(HW_HAS_DRV8323S)
         if (fault_vec[i].fault == FAULT_CODE_DRV) {
            commands_printf("DRV8323S_FAULTS  : %s", drv8323s_faults_to_string(fault_vec[i].drv8301_faults));
         }
   #endif
         commands_printf(" ");
      }
   }
}


static void cmnd_processs_tim(void)
{
   chSysLock();
   volatile int t1_cnt = TIM1->CNT;
   volatile int t8_cnt = TIM8->CNT;
   volatile int t1_cnt2 = TIM1->CNT;
   volatile int t2_cnt = TIM2->CNT;
   volatile int dir1 = !!(TIM1->CR1 & (1 << 4));
   volatile int dir8 = !!(TIM8->CR1 & (1 << 4));
   chSysUnlock();

   int duty1 = TIM1->CCR1;
   int duty2 = TIM1->CCR2;
   int duty3 = TIM1->CCR3;
   int top = TIM1->ARR;
   int voltage_samp = TIM8->CCR1;
   int current1_samp = TIM1->CCR4;
   int current2_samp = TIM8->CCR2;

   commands_printf("Tim1 CNT: %i", t1_cnt);
   commands_printf("Tim8 CNT: %i", t8_cnt);
   commands_printf("Tim2 CNT: %i", t2_cnt);
   commands_printf("Amount off CNT: %i",top - (2*t8_cnt + t1_cnt + t1_cnt2)/2);
   commands_printf("Duty cycle1: %u", duty1);
   commands_printf("Duty cycle2: %u", duty2);
   commands_printf("Duty cycle3: %u", duty3);
   commands_printf("Top: %u", top);
   commands_printf("Dir1: %u", dir1);
   commands_printf("Dir8: %u", dir8);
   commands_printf("Voltage sample: %u", voltage_samp);
   commands_printf("Current 1 sample: %u", current1_samp);
   commands_printf("Current 2 sample: %u\n", current2_samp);
}


static void cmnd_processs_crc(void)
{
   unsigned mc_crc0 = mc_interface_get_configuration()->crc;
   unsigned mc_crc1 = mc_interface_calc_crc(NULL, false);
   unsigned app_crc0 = app_get_configuration()->crc;
   unsigned app_crc1 = app_calc_crc(NULL);
   commands_printf("MC CFG crc: 0x%04X (stored)  0x%04X (recalc)", mc_crc0, mc_crc1);
   commands_printf("APP CFG crc: 0x%04X (stored)  0x%04X (recalc)", app_crc0, app_crc1);
   commands_printf("Discrepancy is expected due to run-time recalculation of config params.\n");
}

#endif  // CMND_MISC_H_
