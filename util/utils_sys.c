/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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

#include "utils_sys.h"
#include "hal.h"
#include "app.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>

// Private variables
static volatile int sys_lock_cnt = 0;

/**
 * A system locking function with a counter. For every lock, a corresponding unlock must
 * exist to unlock the system. That means, if lock is called five times, unlock has to
 * be called five times as well. Note that chSysLock and chSysLockFromIsr are the same
 * for this port.
 */
void utils_sys_lock_cnt(void) {
	if (!sys_lock_cnt) {
		chSysLock();
	}
	sys_lock_cnt++;
}

/**
 * A system unlocking function with a counter. For every lock, a corresponding unlock must
 * exist to unlock the system. That means, if lock is called five times, unlock has to
 * be called five times as well. Note that chSysUnlock and chSysUnlockFromIsr are the same
 * for this port.
 */
void utils_sys_unlock_cnt(void) {
	if (sys_lock_cnt) {
		sys_lock_cnt--;
		if (!sys_lock_cnt) {
			chSysUnlock();
		}
	}
}

/**
 * Get ID of second motor.
 *
 * @return
 * id for second motor. -1 if this hardware only has one motor.
 */
uint8_t utils_second_motor_id(void) {
#ifdef HW_HAS_DUAL_MOTORS
	uint8_t id_next = app_get_configuration()->controller_id + 1;
	if (id_next == 255) {
		id_next = 0;
	}
	return id_next;
#else
	return 0;
#endif
}

/**
 * Read hall sensors
 *
 * @param is_second_motor
 * Use hall sensor port for second motor on dual motor hardware.
 *
 * @param samples
 * The number of extra samples to read and filter over. If this
 * is 0, only one sample will be used.
 *
 * @return
 * The state of the three hall sensors.
 */
int utils_read_hall(bool is_second_motor, int samples) {
	samples = 1 + 2 * samples;

	int h1 = 0, h2 = 0, h3 = 0;
	int tres = samples / 2;

	if (is_second_motor) {
		while (samples--) {
			h1 += READ_HALL1_2();
			h2 += READ_HALL2_2();
			h3 += READ_HALL3_2();
		}
	} else {
		while (samples--) {
			h1 += READ_HALL1();
			h2 += READ_HALL2();
			h3 += READ_HALL3();
		}
	}

	return (h1 > tres) | ((h2 > tres) << 1) | ((h3 > tres) << 2);
}

const char* utils_hw_type_to_string(HW_TYPE hw) {
	switch (hw) {
	case HW_TYPE_VESC: return "HW_TYPE_VESC"; break;
	case HW_TYPE_VESC_BMS: return "HW_TYPE_VESC_BMS"; break;
	case HW_TYPE_CUSTOM_MODULE: return "HW_TYPE_CUSTOM_MODULE"; break;
	default: return "FAULT_HARDWARE"; break;
	}
}

/**
 * Check the minimum stack tp had left by counting the remaining fill characters.
 */
int utils_check_min_stack_left(thread_t *tp) {
	uint32_t *p = (uint32_t *)tp->p_stklimit;

	int free = 0;
	while (free < 8192) {
		if (*p++ != 0x55555555) {
			break;
		}
		free += sizeof(uint32_t);
	}

	return free;
}

/*
 * Check how much stack the current thread has left now.
 */
int utils_stack_left_now(void) {
	struct port_intctx *r13 = (struct port_intctx *)__get_PSP();
	return ((stkalign_t *)(r13 - 1) - chThdGetSelfX()->p_stklimit) * sizeof(stkalign_t);
}

/*
 * Check if function is on a valid address
 */
bool utils_is_func_valid(void *addr) {
	bool res = false;

	// Flash
	if ((uint32_t)addr >= 0x08000000 && (uint32_t)addr <= (0x08000000 + 1024 * 1024)) {
		res = true;
	}

	// Ram
	if ((uint32_t)addr >= 0x20000000 && (uint32_t)addr <= (0x20000000 + 1024 * 128)) {
		res = true;
	}

	// CCM
	if ((uint32_t)addr >= 0x10000000 && (uint32_t)addr <= (0x10000000 + 1024 * 64)) {
		res = true;
	}

	return res;
}
