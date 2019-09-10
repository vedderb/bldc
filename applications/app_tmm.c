/*
    Copyright 2019 Benjamin Vedder  benjamin@vedder.se

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

#include "app.h"
#include "ch.h"
#include "hal.h"

// Some useful includes
#include "mc_interface.h"
#include "utils.h"
#include "encoder.h"
#include "terminal.h"
#include "comm_can.h"
#include "hw.h"
#include "commands.h"

#include <math.h>
#include <string.h>
#include <stdio.h>

// Threads
static THD_FUNCTION(my_thread, arg);
static THD_WORKING_AREA(my_thread_wa, 2048);

// Private functions
static void pwm_callback(void);
static void terminal_info(int argc, const char **argv);

// Private variables
static volatile bool stop_now = true;
static volatile bool is_running = false;
static volatile float min_pwr = 3.3;
static volatile float max_pwr = 0.0;
static volatile float current_rel_pwr = 0.0;
static volatile uint16_t cycles = 0;

// Called when the custom application is started. Start our
// threads here and set up callbacks.
void app_custom_start(void) {
    mc_interface_set_pwm_callback(pwm_callback);

    stop_now = false;
    chThdCreateStatic(my_thread_wa, sizeof(my_thread_wa),
            NORMALPRIO, my_thread, NULL);

    // Terminal commands for the VESC Tool terminal can be registered.
    terminal_register_command_callback(
            "app_tmm",
            "show variables",
            "",
            terminal_info);
}

// Called when the custom application is stopped. Stop our threads
// and release callbacks.
void app_custom_stop(void) {
    mc_interface_set_pwm_callback(0);
    terminal_unregister_callback(terminal_info);

    stop_now = true;
    while (is_running) {
        chThdSleepMilliseconds(1);
    }
}

void app_custom_configure(app_configuration *conf) {
    (void)conf;
}

static THD_FUNCTION(my_thread, arg) {
    (void)arg;

    chRegSetThreadName("App Custom");

    is_running = true;

    for(;;) {
        // Check if it is time to stop.
        if (stop_now) {
            is_running = false;
            return;
        }

        // Run your logic here. A lot of functionality is available in mc_interface.h.
        float pwr = (float)ADC_VOLTS(ADC_IND_EXT);
        float brake = (float)ADC_VOLTS(ADC_IND_EXT2);

        /**
         * update mc interface every 2 secounds
         */
        if(cycles == 1000) {
           /**
            * reset cycles counter
            */
           cycles = 0;

           /**
            * Recuperation of energy, when brakelever switch is closed
            */
            if (brake < 0.01) {
              mc_interface_set_brake_current_rel(1.0);
            } else {
              /**
               * auto calibrate sensor
               * min: only when motor is not spinning
               * max: when motor is spinning or user is applying pressure on the pedal 
               */
              if(mc_interface_get_tot_current() == 0.0 && pwr < min_pwr) {
                min_pwr = pwr;
              } else {
                if(pwr > max_pwr) {
                  max_pwr = pwr;
                }

                /**
                 * calculate from voltage range min to max to 
                 * range of 0.0 to 1.0
                 */
                current_rel_pwr = fabs(0.01 *
                (
                    (100 / (max_pwr - min_pwr)) *
                    (pwr - min_pwr)
                ));

                mc_interface_set_current_rel(current_rel_pwr);
              }
            }
        }

        chThdSleepMilliseconds(2);
        cycles++;
    }
}

static void pwm_callback(void) {
    // Called for every control iteration in interrupt context.
}

// Callback function for the terminal command with arguments.
static void terminal_info(int argc, const char **argv) {
      commands_printf(
            "MIN: %.2f V MAX: %.2f V CYCLES: %u CURRENT_REL: %u",
                min, max, cylces,current_rel_pwr);
}

