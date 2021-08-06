/*
	Copyright 2017 Benjamin Vedder	benjamin@vedder.se
        Copyright 2021 Sam Shum         cshum@andrew.cmu.edu

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

#include "ch.h" // ChibiOS
#include "hal.h" // ChibiOS HAL
#include "mc_interface.h" // Motor control functions
#include "hw.h" // Pin mapping on this hardware
#include "timeout.h" // To reset the timeout

#include "mcpwm_foc.h" // Motor control functions
#include "timer.h" // Motor control functions

static volatile bool stop_now = true;
static volatile bool is_running = false;
// custom script thread
static THD_FUNCTION(custom_script_thread, arg);
static THD_WORKING_AREA(custom_script_thread_wa, 2048); // 2kb stack for this thread


void app_custom_start(void) {
        if (!is_running)
        {
                stop_now = false;
                is_running = true;
		chThdCreateStatic(custom_script_thread_wa, sizeof(custom_script_thread_wa), NORMALPRIO, custom_script_thread, NULL);
        }
}

void app_custom_stop(void) {

     stop_now = true;
     is_running = false;
     mc_interface_release_motor(); 
	
}

void app_custom_configure(app_configuration *conf) {
	(void)conf;
}
 
static THD_FUNCTION(custom_script_thread, arg) 
{
		is_running = true;
		(void)arg;
	 
		chRegSetThreadName("APP_PLAY_TUNES");
 
                //save original switching frequency
                float original_sw = mc_interface_get_configuration()->foc_f_sw;

                //play the startup tune
                for( int a = 3000; a < 5000; a = a + 500 ) {
                     change_sw(a);
                     mc_interface_set_pid_speed(1000.0);
                     chThdSleepMilliseconds(300);
                     timeout_reset();
                }
                    
                //stop playing
                mc_interface_set_pid_speed(0.0);
                chThdSleepMilliseconds(2000);
                timeout_reset();

                //play the error/fault tune
                for( int i = 0; i < 3; i++ ) {
                     change_sw(5000);
                     chThdSleepMilliseconds(300);
                     mc_interface_set_pid_speed(1000.0);
                     chThdSleepMilliseconds(700);
                     timeout_reset();
                }

                //stop playing
                change_sw((int)original_sw);
                mc_interface_set_pid_speed(0.0);
                chThdSleepMilliseconds(500);
                timeout_reset();

         
                is_running = false;
                chThdExit(0);   // terminate the thread, all scripted behavior completed
		
}

