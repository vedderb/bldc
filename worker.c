/*
	Copyright 2020 Benjamin Vedder	benjamin@vedder.se

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

#include "worker.h"
#include "ch.h"
#include "hal.h"

// Private types
typedef struct {
	void *arg;
	void(*func)(void *arg);
} worker_arg_t;

// Private variables
static thread_t *m_tp = 0;
static worker_arg_t m_wa;
static THD_WORKING_AREA(work_thread_wa, 512);
static THD_FUNCTION(work_thread, arg);

void worker_execute(void(*func)(void *arg), void *arg) {
	worker_wait();
	m_wa.func = func;
	m_wa.arg = arg;
	chThdCreateStatic(work_thread_wa, sizeof(work_thread_wa), NORMALPRIO, work_thread, &m_wa);
}

void worker_wait(void) {
	if (m_tp) {
		chThdWait(m_tp);
	}
}

static THD_FUNCTION(work_thread, arg) {
	chRegSetThreadName("Worker");
	m_tp = chThdGetSelfX();
	((worker_arg_t*)arg)->func(((worker_arg_t*)arg)->arg);
	m_tp = 0;
}
