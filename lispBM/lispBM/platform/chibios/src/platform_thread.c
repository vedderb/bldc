/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "platform_thread.h"
#include "lbm_memory.h"
#include <stdint.h>

THD_FUNCTION(platform_chibios_thd_fun, arg) {
  platform_thread_t *pt = (platform_thread_t*)arg;
  pt->tfun(pt->arg);
}

bool lbm_thread_create(lbm_thread_t *t,
                       const char *name,
                       lbm_thread_func_t func,
                       void *arg,
                       lbm_thread_prio_t prio,
                       uint32_t stack_size) {
  (void)name;

  platform_thread_t *thread = (platform_thread_t*)t;
  if (!thread) return false;

  thread->arg = arg;
  thread->tfun = func;
  
  uint32_t wa_size = THD_WORKING_AREA_SIZE(stack_size);

  void *wa = lbm_malloc(wa_size + 8);
  if (!wa) {
    return false;
  }

  thread->working_area = wa;
  thread->wa_size = wa_size;

  uintptr_t addr = (uintptr_t)wa;
  uintptr_t aligned_addr = (addr + 7) & ~7;
  void *aligned_wa = (void *)aligned_addr;

  thread->handle = chThdCreateStatic(aligned_wa, wa_size, prio,
                                     platform_chibios_thd_fun, (void*)thread);

  if (!thread->handle) {
    lbm_free(wa);
    return false;
  }

  return true;
}

void lbm_thread_sleep_us(uint32_t microseconds) {
  chThdSleepMicroseconds(microseconds);
}

void lbm_thread_yield(void) {
  chThdYield();
}

void lbm_thread_destroy(lbm_thread_t *t) {
  if (t) {
    platform_thread_t *thread = t;

    if (thread->handle) {
      chThdWait(thread->handle);
    }

    if (thread->working_area) {
      lbm_free(thread->working_area);
    }
  }
}
