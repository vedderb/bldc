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

#ifndef CMND_HELP_H_
#define CMND_HELP_H_

#include "terminal.h"
#include "commands.h"

// Functions
static void cmnd_processs_help(int callback_write, terminal_callback_struct *callbacks)
{
   commands_printf("Valid commands are:");
   commands_printf("help");
   commands_printf("  Show this help");

   commands_printf("ping");
   commands_printf("  Print pong here to see if the reply works");

   commands_printf("stop");
   commands_printf("  Stop the motor");

   commands_printf("last_adc_duration");
   commands_printf("  The time the latest ADC interrupt consumed");

   commands_printf("kv");
   commands_printf("  The calculated kv of the motor");

   commands_printf("mem");
   commands_printf("  Show memory usage");

   commands_printf("threads");
   commands_printf("  List all threads");

   commands_printf("fault");
   commands_printf("  Prints the current fault code");

   commands_printf("faults");
   commands_printf("  Prints all stored fault codes and conditions when they arrived");

   commands_printf("rpm");
   commands_printf("  Prints the current electrical RPM");

   commands_printf("tacho");
   commands_printf("  Prints tachometer value");

   commands_printf("dist");
   commands_printf("  Prints odometer value");

   commands_printf("tim");
   commands_printf("  Prints tim1 and tim8 settings");

   commands_printf("volt");
   commands_printf("  Prints different voltages");

   commands_printf("param_detect [current] [min_rpm] [low_duty]");
   commands_printf("  Spin up the motor in COMM_MODE_DELAY and compute its parameters.");
   commands_printf("  This test should be performed without load on the motor.");
   commands_printf("  Example: param_detect 5.0 600 0.06");

   commands_printf("rpm_dep");
   commands_printf("  Prints some rpm-dep values");

   commands_printf("can_devs");
   commands_printf("  Prints all CAN devices seen on the bus the past second");

   commands_printf("foc_encoder_detect [current]");
   commands_printf("  Run the motor at 1Hz on open loop and compute encoder settings");

   commands_printf("measure_res [current]");
   commands_printf("  Lock the motor with a current and calculate its resistance");

   commands_printf("measure_ind [duty]");
   commands_printf("  Send short voltage pulses, measure the current and calculate the motor inductance");

   commands_printf("measure_linkage [current] [duty] [min_erpm] [motor_res]");
   commands_printf("  Run the motor in BLDC delay mode and measure the flux linkage");
   commands_printf("  example measure_linkage 5 0.5 700 0.076");
   commands_printf("  tip: measure the resistance with measure_res first");

   commands_printf("measure_res_ind");
   commands_printf("  Measure the motor resistance and inductance with an incremental adaptive algorithm.");

   commands_printf("measure_linkage_foc [duty]");
   commands_printf("  Run the motor with FOC and measure the flux linkage.");

   commands_printf("measure_linkage_openloop [current] [duty] [erpm_per_sec] [motor_res] [motor_ind]");
   commands_printf("  Run the motor in openloop FOC and measure the flux linkage");
   commands_printf("  example measure_linkage 5 0.5 1000 0.076 0.000015");
   commands_printf("  tip: measure the resistance with measure_res first");

   commands_printf("foc_state");
   commands_printf("  Print some FOC state variables.");

   commands_printf("hw_status");
   commands_printf("  Print some hardware status information.");

   commands_printf("foc_openloop [current] [erpm]");
   commands_printf("  Create an open loop rotating current vector.");

   commands_printf("foc_openloop_duty [duty] [erpm]");
   commands_printf("  Create an open loop rotating voltage vector.");

   commands_printf("nrf_ext_set_enabled [enabled]");
   commands_printf("  Enable or disable external NRF51822.");

   commands_printf("foc_sensors_detect_apply [current]");
   commands_printf("  Automatically detect FOC sensors, and apply settings on success.");

   commands_printf("rotor_lock_openloop [current_A] [time_S] [angle_DEG]");
   commands_printf("  Lock the motor with a current for a given time. Time 0 means forever, or");
   commands_printf("  or until the heartbeat packets stop.");

   commands_printf("foc_detect_apply_all [max_power_loss_W]");
   commands_printf("  Detect and apply all motor settings, based on maximum resistive motor power losses.");

   commands_printf("can_scan");
   commands_printf("  Scan CAN-bus using ping commands, and print all devices that are found.");

   commands_printf("foc_detect_apply_all_can [max_power_loss_W]");
   commands_printf("  Detect and apply all motor settings, based on maximum resistive motor power losses. Also");
   commands_printf("  initiates detection in all VESCs found on the CAN-bus.");

   commands_printf("encoder");
   commands_printf("  Prints the status of the AS5047, AD2S1205, or TS5700N8501 encoder.");

   commands_printf("encoder_clear_errors");
   commands_printf("  Clear error of the TS5700N8501 encoder.)");

   commands_printf("encoder_clear_multiturn");
   commands_printf("  Clear multiturn counter of the TS5700N8501 encoder.)");

   commands_printf("uptime");
   commands_printf("  Prints how many seconds have passed since boot.");

   commands_printf("hall_analyze [current]");
   commands_printf("  Rotate motor in open loop and analyze hall sensors.");

   commands_printf("io_board_set_output [id] [ch] [state]");
   commands_printf("  Set digital output of IO board.");

   commands_printf("io_board_set_output_pwm [id] [ch] [duty]");
   commands_printf("  Set pwm output of IO board.");

   commands_printf("crc");
   commands_printf("  Print CRC values.");

   for (int i = 0;i < callback_write;i++) {
      if (callbacks[i].cbf == 0) {
         continue;
      }

      if (callbacks[i].arg_names) {
         commands_printf("%s %s", callbacks[i].command, callbacks[i].arg_names);
      } else {
         commands_printf(callbacks[i].command);
      }

      if (callbacks[i].help) {
         commands_printf("  %s", callbacks[i].help);
      } else {
         commands_printf("  There is no help available for this command.");
      }
   }

   commands_printf(" ");
}

#endif  // CMND_HELP_H_
