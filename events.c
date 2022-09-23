/*
	Copyright 2021 Benjamin Vedder	benjamin@vedder.se

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

#include "events.h"
#include "terminal.h"
#include "commands.h"
#include "utils_sys.h"
#include "ch.h"
#include <string.h>
#include <math.h>

// Settings
#define EVENTS_LEN	30

// Private types
typedef struct {
	const char *name;
	thread_t *thread;
	float param;
	systime_t time;
	bool set;
} event_t;

// Private variables
static volatile event_t m_events[EVENTS_LEN];
static volatile int m_event_now = 0;
static mutex_t m_mtx;

// Private functions
static void terminal_print(int argc, const char **argv);

void events_init(void) {
	chMtxObjectInit(&m_mtx);

	for (int i = 0;i < EVENTS_LEN;i++) {
		volatile event_t *e = &m_events[i];
		e->set = false;
	}

	terminal_register_command_callback(
			"events",
			"Print recent motor events",
			0,
			terminal_print);
}

void events_add(const char *name, float param) {
	chMtxLock(&m_mtx);

	int event = m_event_now;

	event--;
	if (event < 0) {
		event = EVENTS_LEN - 1;
	}
	volatile event_t *e = &m_events[event];

	// Just update the last event if it looks like
	// a repeated command. Otherwise the buffer will
	// fill too fast.
	if (e->name != name || // Comparing memory location is enough
			e->thread != chThdGetSelfX() ||
			UTILS_AGE_S(e->time) > 0.2 ||
			(fabsf(param) > 1e-4) != (fabsf(e->param) > 1e-4)) {
		event = (event + 1) % EVENTS_LEN;
		e = &m_events[event];
	}

	e->name = name;
	e->thread = chThdGetSelfX();
	e->param = param;
	e->time = chVTGetSystemTimeX();
	e->set = true;

	event = (event + 1) % EVENTS_LEN;
	m_event_now = event;

	chMtxUnlock(&m_mtx);
}

static void terminal_print(int argc, const char **argv) {
	(void)argc; (void)argv;

	int event = m_event_now;
	int print_cnt = 0;

	do {
		volatile event_t *e = &m_events[event];

		if (e->set) {
			print_cnt++;
			commands_printf("Age    : %.2f s", (double)UTILS_AGE_S(e->time));
			commands_printf("Thread : %s", e->thread->p_name);
			commands_printf("Motor  : %i", e->thread->motor_selected);
			commands_printf("Command: %s", e->name);
			commands_printf("Param  : %.3f\n", (double)e->param);
		}

		event = (event + 1) % EVENTS_LEN;
	} while (event != m_event_now);

	commands_printf("Events total: %d\n", print_cnt);
}
