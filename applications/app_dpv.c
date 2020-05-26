#include "app.h"

#include "ch.h" // ChibiOS
#include "hal.h" // ChibiOS HAL
#include "stm32f4xx_conf.h"
#include "mc_interface.h" // Motor control functions
#include "hw.h" // Pin mapping on this hardware
#include "timeout.h" // To reset the timeout
#include "utils.h"
#include "commands.h"
#include <math.h>
#include "isr_vector_table.h"


#define SPEED_STEP	0.05
#define SPEED_MAX	1.00
#define SPEED_MIN	0.05
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
void dpv_rotary_isr(void);
static void update(void *p);


CH_IRQ_HANDLER(HW_HALL_ROTARY_A_EXTI_ISR_VEC) {
        if (EXTI_GetITStatus(HW_HALL_ROTARY_A_EXTI_LINE) != RESET) {
                dpv_rotary_isr();
                // Clear the EXTI line pending bit
                EXTI_ClearITPendingBit(HW_HALL_ROTARY_A_EXTI_LINE);
        EXTI_ClearFlag(HW_HALL_ROTARY_A_EXTI_LINE);
        }
}


void dpv_rotary_isr(void) {

//	commands_printf("Interrupt");
	if ( palReadPad(HW_HALL_ROTARY_B_GPIO, HW_HALL_ROTARY_B_PIN) ) {
		targetSpeed += SPEED_STEP;
		if (targetSpeed > SPEED_MAX) targetSpeed = SPEED_MAX;
	} else {
		targetSpeed -= SPEED_STEP;
		if (targetSpeed < SPEED_MIN) targetSpeed = SPEED_MIN;
	}
	//commands_printf("Target Speed: %01.2f", targetSpeed);
}


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
        EXTI_InitTypeDef   EXTI_InitStructure;

	stop_now = false;
	// Set the UART TX pin as an input with pulldown
	palSetPadMode(HW_HALL_TRIGGER_GPIO, HW_HALL_TRIGGER_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ROTARY_A_GPIO, HW_HALL_ROTARY_A_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ROTARY_B_GPIO, HW_HALL_ROTARY_B_PIN, PAL_MODE_INPUT_PULLUP);

        // Interrupt on HALL ROTARY A Pin
        // Connect EXTI Line to pin
        SYSCFG_EXTILineConfig(HW_HALL_ROTARY_A_EXTI_PORTSRC, HW_HALL_ROTARY_A_EXTI_PINSRC);

        // Configure EXTI Line
        EXTI_InitStructure.EXTI_Line = HW_HALL_ROTARY_A_EXTI_LINE;
        EXTI_InitStructure.EXTI_Mode = EXTI_Mode_Interrupt;
        EXTI_InitStructure.EXTI_Trigger = EXTI_Trigger_Rising;
        EXTI_InitStructure.EXTI_LineCmd = ENABLE;
        EXTI_Init(&EXTI_InitStructure);

        // Enable and set EXTI Line Interrupt to the highest priority
        nvicEnableVector(HW_HALL_ROTARY_A_EXTI_CH,6) ;


	// Start the dv thread
	chThdCreateStatic(dpv_thread_wa, sizeof(dpv_thread_wa), NORMALPRIO, dpv_thread, NULL);

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

	chRegSetThreadName("APP_DPV");
	dpv_tp = chThdGetSelfX();


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
//       	mc_interface_set_duty(utils_map(motorSpeed, 0, 1.0, 0, mcconf->l_max_duty));
//       	mc_interface_set_pid_speed(utils_map(motorSpeed, 0, 1.0, 0, MAX_ERPM);
           	mc_interface_set_pid_speed(motorSpeed*mcconf->l_max_erpm);
		// Reset the timeout
		timeout_reset();
	}
}
