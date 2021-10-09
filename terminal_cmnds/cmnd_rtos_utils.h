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

#ifndef CMND_RTOS_UTILS_H_
#define CMND_RTOS_UTILS_H_

#include "terminal.h"
#include "commands.h"
#include "ch.h"

// Functions

static void cmnd_processs_mem(void)
{
   size_t n, size;
   n = chHeapStatus(NULL, &size);
   commands_printf("core free memory : %u bytes", chCoreGetStatusX());
   commands_printf("heap fragments   : %u", n);
   commands_printf("heap free total  : %u bytes\n", size);
}


static void cmnd_processs_threads(void)
{
   thread_t *tp;
   static const char *states[] = {CH_STATE_NAMES};
   commands_printf("    addr    stack prio refs     state           name motor time    ");
   commands_printf("-------------------------------------------------------------------");
   tp = chRegFirstThread();
   do {
      commands_printf("%.8lx %.8lx %4lu %4lu %9s %14s %5lu %lu (%.1f %%)",
            (uint32_t)tp, (uint32_t)tp->p_ctx.r13,
            (uint32_t)tp->p_prio, (uint32_t)(tp->p_refs - 1),
            states[tp->p_state], tp->p_name, tp->motor_selected, (uint32_t)tp->p_time,
            (double)(100.0 * (float)tp->p_time / (float)chVTGetSystemTimeX()));
      tp = chRegNextThread(tp);
   } while (tp != NULL);
   commands_printf(" ");
}


#endif  // CMND_RTOS_UTILS_H_
