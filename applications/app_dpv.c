#include "app.h"

#include "ch.h" // ChibiOS
#include "hal.h" // ChibiOS HAL
#include "stm32f4xx_conf.h"
#include "mc_interface.h" // Motor control functions
#include "hw.h" // Pin mapping on this hardware
#include "timeout.h" // To reset the timeout
#include "utils_math.h"
#include "commands.h"
#include <math.h>
#include "isr_vector_table.h"
#include "stdlib.h"

#define SPEED_STEP	0.05
#define SPEED_MAX	1.00
#define SPEED_MIN	0.10
#define SPEED_OFF	0.00

//private variables
static volatile bool stop_now = true;
static volatile bool is_running = false;
static float targetSpeed=SPEED_MIN;

//Threads
static THD_FUNCTION(dpv_thread, arg);
static THD_WORKING_AREA(dpv_thread_wa, 2048); // 2kb stack for this thread
static thread_t *dpv_tp;
virtual_timer_t dpv_vt;

//private functions
static void update(void *p);

void app_custom_configure(app_configuration *conf)
{
	(void)conf;
}

void app_custom_stop(void) 
{
        stop_now = true;

        if (is_running) {
		chEvtSignalI(dpv_tp, (eventmask_t) 1);
		mc_interface_release_motor();
        }

        while (is_running) {
                chThdSleepMilliseconds(1);
        }

}

void app_custom_start(void) {

	stop_now = false;
    //Config GPIO
	palSetPadMode(HW_HALL_TRIGGER_GPIO, HW_HALL_TRIGGER_PIN, PAL_MODE_INPUT_PULLUP);

	// Start the dv thread
	chThdCreateStatic(dpv_thread_wa, sizeof(dpv_thread_wa), NORMALPRIO, dpv_thread, NULL);

    hw_start_i2c();
    app_uartcomm_start(UART_PORT_COMM_HEADER);
    app_uartcomm_start(UART_PORT_BUILTIN);
    chSysLock();
    chVTSetI(&dpv_vt, MS2ST(1), update, NULL);
    chSysUnlock();
}

static void update(void *p) {
        if (!is_running) {
                return;
        }

        chSysLockFromISR();
        chVTSetI(&dpv_vt, MS2ST(2), update, p);
        chEvtSignalI(dpv_tp, (eventmask_t) 1);
        chSysUnlockFromISR();
}

static THD_FUNCTION(dpv_thread, arg) {
	(void)arg;

    int angle=0, oldangle=0;

	chRegSetThreadName("APP_DPV");
	dpv_tp = chThdGetSelfX();

    angle = gsvesc_get_angle();
    oldangle=angle;

    is_running = true;
	for(;;) {
	        chEvtWaitAny((eventmask_t) 1);
                if (stop_now) {
                        is_running = false;
                        return;
                }

		const volatile mc_configuration *mcconf = mc_interface_get_configuration();
		float motorSpeed=SPEED_MIN;

		if (app_is_output_disabled() ) {
			continue;
		}

		if ( mc_interface_get_fault() != FAULT_CODE_NONE) {
			continue;
		}

        // Get position of speed poti
        angle = gsvesc_get_angle();

        if (abs(oldangle - angle) > 1000) {    // sprung ?
            angle = oldangle;
        } else {
            oldangle = angle;
        }
        if (angle > 3800) angle = 3800;
        if (angle < 400) angle = 400;
        targetSpeed= utils_map(angle,400,3800,SPEED_MIN,SPEED_MAX);
        // Apply ramping

        static systime_t last_time = 0;
        static float motorSpeed_val_ramp = 0.0;
	float ramp_time; 
	if ( ! palReadPad(HW_HALL_TRIGGER_GPIO, HW_HALL_TRIGGER_PIN)) {
		motorSpeed=targetSpeed;
	} else {
		motorSpeed=SPEED_OFF;
	}
  	ramp_time = fabsf(motorSpeed) > fabsf(motorSpeed_val_ramp) ? 5.0 : 0.5;
    	if (fabsf(motorSpeed) > 0.01) {
        	ramp_time = fminf(3.0, 3.0);
        }
   	if (ramp_time > 0.01) {
		const float ramp_step = (float)ST2MS(chVTTimeElapsedSinceX(last_time)) / (ramp_time * 1000.0);
        	utils_step_towards(&motorSpeed_val_ramp, motorSpeed, ramp_step);
        	last_time = chVTGetSystemTimeX();
		motorSpeed = motorSpeed_val_ramp;
       	}
       	//mc_interface_set_pid_speed(motorSpeed*mcconf->l_max_erpm);
        mc_interface_set_duty(motorSpeed*mcconf->l_max_duty);
	chThdSleepMilliseconds(5);
	// Reset the timeout
	timeout_reset();
	}
}
