/*
	Copyright 2026 Lukas Hrazky

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

#include "imu_thread.h"
#include "drdy.h"
#include "ch.h"
#include "terminal.h"
#include "commands.h"
#include "utils_math.h"

#include <stdio.h>

// In DRDY mode, fall back to a timed read after this many sample periods
// without a data-ready edge, so a missed edge or an unwired pin can't stall
// the loop.
#define DRDY_TIMEOUT_PERIODS 2

static THD_FUNCTION(thread_func, arg);

static stkalign_t m_wa[THD_WORKING_AREA_SIZE(1024) / sizeof(stkalign_t)];
static thread_t *m_thd = NULL;
static imu_device_t *m_dev;
static volatile uint32_t m_read_fails;
static bool m_drdy_active = false;
static void (*m_cb)(float *accel, float *gyro, float *mag);
static bool m_cmds_registered = false;

static void terminal_read_reg(int argc, const char **argv) {
	if (argc != 2) {
		commands_printf("This command requires one argument.");
		return;
	}

	if (!m_dev) {
		commands_printf("No active IMU.");
		return;
	}

	int reg = -1;
	sscanf(argv[1], "%i", &reg); // %i auto-detects base: 0x.. hex, else decimal
	if (reg < 0 || reg > 0xFF) {
		commands_printf("Invalid argument(s).");
		return;
	}

	uint8_t val = 0;
	if (transport_read_reg(m_dev->transport, m_dev->dev_addr, reg, &val, 1)) {
		char bl[9];
		utils_byte_to_binary(val, bl);
		commands_printf("Reg 0x%02x: 0x%02x (0b%s)", reg, val, bl);
	} else {
		commands_printf("Read failed.");
	}
}

static void terminal_status(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	if (!m_dev) {
		commands_printf("No active IMU.");
		return;
	}

	commands_printf("Model         : %s", m_dev->interface->name);
	if (m_dev->variant) {
		commands_printf("Variant       : %s", m_dev->variant);
	}
	commands_printf(
			"Transport     : %s\n"
			"Running       : %s\n"
			"Sample Rate   : %d Hz\n"
			"Read fails    : %u",
			m_dev->transport->interface->name,
			m_thd ? "yes" : "no",
			m_dev->sample_rate_hz,
			m_read_fails);

	if (m_drdy_active) {
		commands_printf(
				"DRDY ints     : %u\n"
				"DRDY timeouts : %u\n",
				drdy_interrupt_count(),
				drdy_timeout_count());
	}
}

void imu_thread_set_device(imu_device_t *dev, uint16_t rate_hz) {
	m_dev = dev;
	m_dev->sample_rate_hz = rate_hz;
	m_read_fails = 0;

	// Interrupt mode only when both the board wires a DRDY pin and the device can route its
	// data-ready to it; otherwise the timed loop runs and the hook is never called. Resolved
	// here (before configure()) so the device's configure() can adapt its ODR/filter setup.
	dev->use_drdy = drdy_present() && dev->interface->enable_drdy_output != NULL;

	if (!m_cmds_registered) {
		terminal_register_command_callback(
				"imu_read_reg",
				"Read a register of the active IMU",
				"[reg]",
				terminal_read_reg);
		terminal_register_command_callback(
				"imu_status",
				"Print status of the active IMU",
				0,
				terminal_status);
		m_cmds_registered = true;
	}
}

void imu_thread_start(void (*cb)(float *accel, float *gyro, float *mag)) {
	m_cb = cb;

	m_drdy_active = m_dev->use_drdy;
	if (m_drdy_active) {
		drdy_init();
		m_dev->interface->enable_drdy_output(m_dev, true);
	}

	m_thd = chThdCreateStatic(m_wa, sizeof(m_wa), NORMALPRIO, thread_func, NULL);
}

void imu_thread_stop(void) {
	if (m_thd) {
		chThdTerminate(m_thd);
		drdy_signal(); // unblock a DRDY wait so the thread sees the terminate flag
		chThdWait(m_thd);
		m_thd = NULL;
	}

	if (m_dev && m_drdy_active) {
		m_dev->interface->enable_drdy_output(m_dev, false);
		drdy_deinit();
	}

	m_dev = NULL;
	m_drdy_active = false;
}

static THD_FUNCTION(thread_func, arg) {
	(void)arg;
	chRegSetThreadName("IMU");

	const systime_t period = US2ST(1000000 / m_dev->sample_rate_hz);
	// One tick less so the actual rate is at least the configured one (US2ST rounds up).
	const systime_t interval = period - 1;

	while (!chThdShouldTerminateX()) {
		systime_t start_time = chVTGetSystemTimeX();

		if (m_drdy_active) {
			drdy_wait(DRDY_TIMEOUT_PERIODS * period);
			if (chThdShouldTerminateX()) {
				break;
			}
		}

		float accel[3], gyro[3], mag[3];
		if (!m_dev->interface->read_sample(m_dev, accel, gyro, mag)) {
			m_read_fails++;
			if (m_dev->interface->on_read_fail) {
				m_dev->interface->on_read_fail(m_dev);
			}
			// Always yield at least a tick so a lightweight on_read_fail can't busy-spin.
			chThdSleep(1);
			continue;
		}

		if (m_cb) {
			m_cb(accel, gyro, mag);
		}

		// In DRDY mode the next edge wakes the loop, we only sleep in timed mode
		if (!m_drdy_active) {
			systime_t sleep_ticks = 1;
			systime_t remaining = start_time + interval - chVTGetSystemTimeX();
			if (remaining > 0 && remaining <= interval) {
				sleep_ticks = remaining;
			}
			chThdSleep(sleep_ticks);
		}
	}
}
