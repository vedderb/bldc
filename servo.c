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

#include "servo.h"
#include <stdarg.h>
#include <stdlib.h>

#include "ch.h"
#include "hal.h"

#if SERVO_OUT_ENABLE && !SERVO_OUT_SIMPLE

volatile SERVO servos[SERVOS_NUM];

#if USE_COMMANDS
volatile SERVO_CMD commands[SERVOS_NUM];
volatile unsigned int cmd_counter = 0, cmd_time_to_run = 0;
volatile unsigned char cmd_repeat = 0;
#endif

#if TEST_CYCLE_TIME
	volatile unsigned int restart_cnt = 0;
	volatile unsigned int interrupt_cnt = 0;
#endif

static SERVO sorted_servos[SERVOS_NUM];
static volatile signed char int_index;
static volatile signed char servo_int_index;
static volatile signed char masks_ports_index;
static volatile unsigned char driver_active;
static volatile unsigned int delays[SERVOS_NUM + 1];
static volatile unsigned char same_pos[SERVOS_NUM + 1];
static volatile unsigned char same_bits[SERVOS_NUM + 1];
static volatile unsigned int masks[SERVOS_NUM];
static stm32_gpio_t *ports[SERVOS_NUM];
static volatile unsigned char length;
static size_t servo_struct_size = sizeof(SERVO);
static size_t sorted_servos_len = sizeof(sorted_servos) / sizeof(SERVO);

static thread_t *servo_tp;
static THD_WORKING_AREA(servo_thread_wa, 512);
static THD_FUNCTION(servo_thread, arg);

/*
 * Private functions
 */
static void servo_get_copy(SERVO *a);
static void servo_init_timer(void);
static void servo_start_pulse(void);

/*
 * HW-specific START
 */
#define SERVO_CNT		TIM7->CNT
#define SERVO_CNT_TOP	TIM7->ARR

static void servo_init_timer(void) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	uint16_t PrescalerValue = 0;
	NVIC_InitTypeDef NVIC_InitStructure;

	// ------------- Timer7 ------------- //
	// Compute the prescaler value
	// TIM7 clock enable
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM7, ENABLE);

	PrescalerValue = (uint16_t) ((SYSTEM_CORE_CLOCK / 2) / SERVO_CNT_SPEED) - 1;

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM7, &TIM_TimeBaseStructure);

	// Prescaler configuration
	TIM_PrescalerConfig(TIM7, PrescalerValue, TIM_PSCReloadMode_Immediate);

	// Disable ARR buffering
	TIM_ARRPreloadConfig(TIM7, DISABLE);

	// Interrupt generation
	TIM_ITConfig(TIM7, TIM_IT_Update, ENABLE);

	// TIM6 enable counter
	TIM_Cmd(TIM7, ENABLE);

	// NVIC
	NVIC_InitStructure.NVIC_IRQChannel = TIM7_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
}

void servo_init(void) {
	hw_setup_servo_outputs();

	for (int i = 0; i < SERVOS_NUM; i++) {
		palSetPadMode(servos[i].gpio, servos[i].pin, PAL_MODE_OUTPUT_PUSHPULL |
				PAL_STM32_OSPEED_HIGHEST);
		palClearPad(servos[i].gpio, servos[i].pin);
	}

	int_index = 0;
	length = 0;

#if USE_COMMANDS
	unsigned char i;
	for (i = 0; i < SERVOS_NUM; i++) {
		commands[i].active = 0;
		commands[i].last = 0;
	}
#endif

	chThdCreateStatic(servo_thread_wa, sizeof(servo_thread_wa), NORMALPRIO, servo_thread, NULL);

	servo_init_timer();
	driver_active = 1;
}

static void servo_start_pulse(void) {
	unsigned char i;
	for (i = 0; i < SERVOS_NUM; i++) {
		palSetPad(servos[i].gpio, servos[i].pin);
	}
}

/*
 * Stop servo driver
 */
void servo_stop_driver(void) {
	// Disable clock
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, DISABLE);

	TIM_Cmd(TIM7, DISABLE);
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM7, DISABLE);

	driver_active = 0;
}
/*
 * HW-specific END
 */

void servo_irq(void) {
	if (int_index < 0) {
		// Start pulse
		servo_start_pulse();
		SERVO_CNT_TOP = SERVO_START_OFFSET + delays[0];
		int_index = 0;
		return;
	}

	if (int_index == length) {
		// Start Cooldown
		SERVO_CNT_TOP = SERVO_COOLDOWN_FACTOR;
		chSysLockFromISR();
		chEvtSignalI(servo_tp, (eventmask_t) 1);
		chSysUnlockFromISR();
		return;
	}

	// End pulse
	while (same_pos[int_index]--) {
		palClearPort(ports[masks_ports_index], masks[masks_ports_index]);
		servo_int_index += same_bits[masks_ports_index];
		masks_ports_index++;
	}

	int_index++;

	SERVO_CNT_TOP = delays[int_index];


#if TEST_CYCLE_TIME
	if (int_index < length && int_index > 0) {
		interrupt_cnt = SERVO_CNT;
	}
#endif
}

static int servo_cmp_by_pos(const void *a, const void *b) {
	SERVO *ia = (SERVO *) a;
	SERVO *ib = (SERVO *) b;

	if (ACTUAL_POS_PTR(ia) == ACTUAL_POS_PTR(ib)) {
		return 0;
	} else if (ACTUAL_POS_PTR(ia) < ACTUAL_POS_PTR(ib)) {
		return -1;
	} else {
		return 1;
	}
}

static THD_FUNCTION(servo_thread, arg) {
	(void)arg;

	chRegSetThreadName("Servo");

	servo_tp = chThdGetSelfX();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

#if TEST_CYCLE_TIME
		unsigned int pwm_start = SERVO_CNT;
#endif

		servo_int_index = 0;
		masks_ports_index = 0;

		servo_get_copy(sorted_servos);
		qsort(sorted_servos, sorted_servos_len, servo_struct_size, servo_cmp_by_pos);

		delays[0] = ACTUAL_POS(sorted_servos[0]) * SERVO_CPU_FACTOR;
		masks[0] = _BV(sorted_servos[0].pin);
		ports[0] = sorted_servos[0].gpio;

		unsigned short i = 0, j = 1, k = 0;

		for (i = 0; i <= SERVOS_NUM; i++) {
			same_pos[i] = 1;
			same_bits[i] = 1;
		}

		i = 0;

		for(;;) {
			while (ACTUAL_POS(sorted_servos[j]) == ACTUAL_POS(sorted_servos[j - 1]) && j < SERVOS_NUM) {
				if (sorted_servos[j].gpio == sorted_servos[j - 1].gpio) {
					masks[k] |= _BV(sorted_servos[j].pin);
					same_bits[k]++;
				} else {
					same_pos[i]++;
					ports[++k] = sorted_servos[j].gpio;
					masks[k] = _BV(sorted_servos[j].pin);
				}

				j++;
			}

			i++;

			if (j < SERVOS_NUM) {
				delays[i] = (ACTUAL_POS(sorted_servos[j]) - ACTUAL_POS(sorted_servos[j - 1])) * SERVO_CPU_FACTOR;
				ports[++k] = sorted_servos[j].gpio;
				masks[k] = _BV(sorted_servos[j].pin);
				j++;
			} else {
				break;
			}
		}

		/*                                                                            |
		 * Add a few extra cycles here to make sure the interrupt is able to finish. \|/
		 */
		delays[i] = (ACTUAL_POS(sorted_servos[SERVOS_NUM - 1])) * SERVO_CPU_FACTOR + 120;
		length = i;

#if USE_COMMANDS
		/*
		 * Run the commands for the servos here.
		 */
		for (i = 0;i < SERVOS_NUM;i++) {
			if (commands[i].active) {
				signed short p = commands[i].pos, ps = servos[i].pos;

				if (p == ps) {
					commands[i].active = 0;
					commands[i].last = 0;
					continue;
				}

				commands[i].last += commands[i].speed;
				signed short delta = commands[i].last >> 5;

				if (p < ps) {
					servos[i].pos -= delta;
					if (delta > (ps - p)) {
						servos[i].pos = p;
					}
				} else {
					servos[i].pos += delta;
					if (delta > (p - ps)) {
						servos[i].pos = p;
					}
				}

				//if (delta > 0) {
				//	commands[i].last = 0;
				//}
				commands[i].last -= delta << 5;
			}
		}

		cmd_counter++;

		if (cmd_counter == cmd_time_to_run && cmd_seq_running) {
			signed short tmp1 = 0, tmp2 = 0, tmp3 = 0, tmp4 = 0;

			while (cmd_seq_running) {
				switch (cmd_seq[cmd_ptr++]) {
				case CMD_MOVE_SERVO:
					tmp1 = cmd_seq[cmd_ptr++];
					tmp2 = cmd_seq[cmd_ptr++];
					tmp3 = cmd_seq[cmd_ptr++];

					if (tmp3 == 0) {
						commands[tmp1].active = 0;
						servos[tmp1].pos = tmp2;
					} else {
						commands[tmp1].speed = tmp3;
						commands[tmp1].pos = tmp2;
						commands[tmp1].active = 1;
					}
					break;

				case CMD_MOVE_SERVO_REL:
					tmp1 = cmd_seq[cmd_ptr++];
					tmp2 = cmd_seq[cmd_ptr++];
					tmp3 = cmd_seq[cmd_ptr++];

					if (tmp3 == 0) {
						commands[tmp1].active = 0;
						servos[tmp1].pos += tmp2;
					} else {
						commands[tmp1].speed = tmp3;
						commands[tmp1].pos += tmp2;
						commands[tmp1].active = 1;
					}
					break;

				case CMD_MOVE_MULTIPLE_SERVOS:
					tmp4 = cmd_seq[cmd_ptr++];
					tmp3 = cmd_seq[cmd_ptr++];

					for (i = 0; i < tmp4; i++) {
						tmp1 = cmd_seq[cmd_ptr++];
						tmp2 = cmd_seq[cmd_ptr++];

						servo_move_within_time(tmp1, tmp2, tmp3);
					}
					break;

				case CMD_CENTER_ALL:
					tmp4 = cmd_seq[cmd_ptr++];
					for (i = 0;i < SERVOS_NUM;i++) {
						servo_move_within_time(i, 0, tmp4);
					}
					break;

				case CMD_WAIT:
					cmd_counter = 0;
					cmd_time_to_run = cmd_seq[cmd_ptr++] * CMD_WAIT_FACTOR;
					goto end_cmds;
					break;

				case CMD_WAIT_SERVO:
					tmp1 = cmd_seq[cmd_ptr++];
					if (commands[tmp1].active) {
						cmd_ptr -= 2;
						cmd_counter = 0;
						cmd_time_to_run = 1;
						goto end_cmds;
					}
					break;

				case CMD_WAIT_ALL_SERVOS:
					for (tmp1 = 0; tmp1 < SERVOS_NUM; tmp1++) {
						if (commands[tmp1].active) {
							cmd_ptr--;
							cmd_counter = 0;
							cmd_time_to_run = 1;
							goto end_cmds;
						}
					}
					break;

				case CMD_STOP_DRIVER:
					servo_stop_cmds();
					servo_stop_driver();
					goto end_cmds;
					break;

				case CMD_STOP_CMDS:
					servo_stop_cmds();
					goto end_cmds;
					break;

				case CMD_RESTART:
					cmd_ptr = 0;
					break;

				case CMD_REPEAT:
					tmp1 = cmd_seq[cmd_ptr++];
					cmd_repeat++;
					if (cmd_repeat < tmp1) {
						cmd_ptr = 0;
					}
					break;

				default:
					servo_stop_cmds();
					goto end_cmds;
					break;
				}

			}

			end_cmds: ;
		}

#endif

#if TEST_CYCLE_TIME
		restart_cnt = SERVO_CNT - pwm_start;
#endif

		int_index = -1;
	}
}

#if USE_COMMANDS
volatile signed char cmd_seq_running = 0;
volatile unsigned int cmd_ptr = 0;
volatile const signed short *cmd_seq;

void servo_move(unsigned char servo, signed short position, unsigned char speed) {
	if (speed == 0) {
		servos[servo].pos = position;
		commands[servo].active = 0;
		return;
	}

	commands[servo].speed = speed;
	commands[servo].pos = position;
	commands[servo].active = 1;
}

void servo_run_cmds(const signed short *cmds) {
	cmd_ptr = 0;
	cmd_seq = cmds;
	cmd_seq_running = 1;
	cmd_counter = 0;
	cmd_time_to_run = 1;
	cmd_repeat = 0;
}

void servo_stop_cmds(void) {
	cmd_seq_running = 0;
}

void servo_reset_pos(unsigned char speed) {
	int i;
	for(i = 0;i < SERVOS_NUM;i++) {
		servo_move(i, 0, speed);
	}
}

void servo_wait_for_cmds() {
	while (cmd_seq_running) {}
}

void servo_move_within_time(unsigned char servo, signed short pos, unsigned short time_ms) {
	if (time_ms == 0) {
		servos[servo].pos = pos;
		commands[servo].active = 0;
		return;
	}

	unsigned short diff = abs(pos - servos[servo].pos);

	commands[servo].speed = ((diff * SERVO_PERIOD_TIME_MS) << 1) / (time_ms >> 4);
	commands[servo].pos = pos;
	commands[servo].active = 1;
}

void servo_move_within_time_multiple(unsigned short time_ms, unsigned short num, ...) {
	va_list arguments;
	unsigned char x = 0;

	va_start(arguments, 2 * num);

	for (x = 0; x < num; x++) {
		unsigned short servo = va_arg(arguments, unsigned int);
		unsigned short pos = va_arg(arguments, unsigned int);

		servo_move_within_time(servo, pos, time_ms);
	}
	va_end(arguments);
}
#endif

unsigned char servo_driver_is_active() {
	return driver_active;
}

static void servo_get_copy(SERVO *a) {
	int i;

	for(i = 0;i < SERVOS_NUM;i++) {
		a[i].gpio = servos[i].gpio;
		a[i].pin = servos[i].pin;
		a[i].pos = servos[i].pos;
		a[i].offset = servos[i].offset;
	}
}

#endif
