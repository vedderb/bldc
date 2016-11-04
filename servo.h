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

/*
 * servo.h
 *
 *  Created on: 2009-apr-25
 *      Author: Benjamin
 *
 *
 * Changelog:
 * 2013-12-XX
 * - Port to ChibiOS and STM32F4
 * - probably more changes
 *
 * 2011-12-08
 * - Renamed functions
 * - Fixed pulse issue when pulses are close together
 *
 * 2011-07-17
 * - Changed some variables to volatile.
 * - Added another command.
 *
 * 2010-11-13
 * - Calculate the servo timings independent of F_CPU.
 * - Cleanup
 *
 */

#ifndef SERVO_H_
#define SERVO_H_

#include "ch.h"
#include "hal.h"
#include "hw.h"
#include "conf_general.h"
#include "stm32f407xx.h"

#ifndef _BV
#define _BV(bit) (1 << (bit))
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

// Change these parameters
#define SERVOS_NUM	HW_SERVO_NUM		// Number of servos to use

// Servo timer speed in HZ
#define SERVO_CNT_SPEED		1000000L

/*
 * Servo timing (in uS)
 *
 * The default signal to drive servos looks like this:
 *
 * ----1000us----|----1000us----|---20000us----
 * _____________________________
 *               |______________|_____...______
 *
 * -S_STARTPULSE-|--S_PULSELEN--|--S_COOLDOWN--
 *
 * And the default parameters are the following
 * #define S_STARTPULSE		1000L
 * #define S_PULSELEN		1000L
 * #define S_COOLDOWN		20000L
 *
 * You can experiment with these to make your servo move further.
 * For some servos these can be way out of spec.
 *
 * Note that S_PULSELEN is not accurate at all for low F_CPU. However,
 * it will be rounded up to the nearest possible value (hence the strange
 * calculation below)
 *
 */
#define S_STARTPULSE	SERVO_OUT_PULSE_MIN_US
#define S_PULSELEN		(SERVO_OUT_PULSE_MAX_US - SERVO_OUT_PULSE_MIN_US)
#define S_COOLDOWN		(1000000 / SERVO_OUT_RATE_HZ)

/*
 * Dynamic servo parameters
 * Calculated from F_CPU
 */
#define SERVO_START_OFFSET		(SERVO_CNT_SPEED / (1000000L / S_STARTPULSE))
#define SERVO_CPU_FACTOR		((SERVO_CNT_SPEED + ((1000000L / S_PULSELEN) * 256L) - 1L) / ((1000000L / S_PULSELEN) * 256L)) // Round up
#define SERVO_COOLDOWN_FACTOR	(SERVO_CNT_SPEED / (1000000L / S_COOLDOWN))

/*
 * Compile with commands to mode servos with a specified speed
 * to s specified position interrupt driven. Enabling this will
 * use some extra ram and a few bytes of flash.
 */
#define USE_COMMANDS 0

/*
 * Calculate how many clock cycles it takes to generate PWM.
 */
#define TEST_CYCLE_TIME 0

#if TEST_CYCLE_TIME
extern volatile unsigned int restart_cnt;
extern volatile unsigned int interrupt_cnt;

#define get_restart_cycles()	(restart_cnt)
#define get_interrupt_cycles()	(interrupt_cnt)
#endif

// Servo macros
#define ACTUAL_POS(servo)			(MAX(MIN(servo.pos + (signed short)servo.offset, 255), 0))
#define ACTUAL_POS_PTR(servo)		(MAX(MIN(servo->pos + (signed short)servo->offset, 255), 0))
#define CMD_MS_TO_VAL(ms)			((ms) / (((S_STARTPULSE + S_PULSELEN + S_COOLDOWN) * CMD_WAIT_FACTOR) / 1000))
#define SERVO_PERIOD_TIME_MS		((S_STARTPULSE + S_PULSELEN + S_COOLDOWN) / 1000)

// Some servo speed defines
// TODO

typedef struct {
	volatile signed short pos;
	volatile unsigned char offset;
	stm32_gpio_t* gpio;
	volatile unsigned int pin;
} SERVO;

#if USE_COMMANDS
typedef struct {
	volatile signed char active;
	volatile signed short pos;
	volatile signed short speed;
	volatile signed short last;
} SERVO_CMD;

extern volatile signed char cmd_seq_running;
extern volatile unsigned int cmd_ptr;
extern volatile const signed short *cmd_seq;

/*
 * The number of servo cycles to wait for each time unit in the wait command.
 *
 * The wait time can be calculated with:
 * (S_STARTPULSE + S_PULSELEN + S_COOLDOWN) * CMD_WAIT_FACTOR
 *
 */
#define CMD_WAIT_FACTOR		1

/*
 * Servo commands.
 */

/*
 * Move servo to given position with given speed.
 *
 * Param 1: Servo.
 * Param 2: Position.
 * Param 3: Speed. 0 for max speed.
 */
#define CMD_MOVE_SERVO		0

/*
 * Move servo to given relative position with given speed.
 *
 * Param 1: Servo.
 * Param 2: Relative position.
 * Param 3: Speed. 0 for max speed.
 */
#define CMD_MOVE_SERVO_REL		1

/*
 * Move multiple servos such that they arrive at the same time
 *
 * Param 1: Number of servos.
 * Param 2: Time for movement in milliseconds
 * Param 3: Servo 1 index
 * Param 4: Servo 1 pos
 * Param 5: Servo 2 index
 * Param 6: Servo 2 pos
 * ... and so on
 */
#define CMD_MOVE_MULTIPLE_SERVOS		2

/*
 * Move all servos to center position
 *
 * Param 1: Time for movement in milliseconds
 */
#define CMD_CENTER_ALL			3

/*
 * Wait for a while. See configuration for more info.
 *
 * Param 1: Time to wait
 */
#define CMD_WAIT			4

/*
 * Wait for servo to be ready.
 *
 * Param 1: Servo to wait for.
 */
#define CMD_WAIT_SERVO		5

/*
 * Wait for all servo commands to get ready.
 */
#define CMD_WAIT_ALL_SERVOS	6

/*
 * Stop servo driver.
 */
#define CMD_STOP_DRIVER		7

/*
 * End of command.
 */
#define CMD_STOP_CMDS		8

/*
 * Restart this command sequence
 */
#define CMD_RESTART			9

/*
 * Repeat command sequence a number of times
 *
 * Param 1: Number of times to repeat commands.
 */
#define CMD_REPEAT			10
#endif

extern volatile SERVO servos[SERVOS_NUM];

void servo_init(void);
void servo_stop_driver(void);
unsigned char servo_driver_is_active(void);
void servo_irq(void);

#if USE_COMMANDS
void servo_move(unsigned char servo, signed short position, unsigned char speed);
void servo_run_cmds(const signed short *cmds);
void servo_stop_cmds(void);
void servo_reset_pos(unsigned char speed);
void servo_wait_for_cmds(void);
void servo_move_within_time(unsigned char servo, signed short pos, unsigned short time_ms);
void servo_move_within_time_multiple(unsigned short time_ms, unsigned short num, ...);
#endif

#endif /* SERVO_H_ */
