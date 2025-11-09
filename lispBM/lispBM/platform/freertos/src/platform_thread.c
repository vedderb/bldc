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

bool lbm_thread_create(lbm_thread_t *t,
                       const char *name,
                       lbm_thread_func_t func,
                       void *arg,
                       lbm_thread_prio_t prio,
                       uint32_t stack_size) {

  platform_thread_t *thread = (platform_thread_t *)t;
  if (!thread) return false;

  uint32_t stack_words = stack_size / sizeof(StackType_t);
  StackType_t *stack = (StackType_t *)lbm_malloc(stack_size);
  if (!stack) {
    return false;
  }

  thread->stack = stack;
  thread->stack_size = stack_size;

  UBaseType_t freertos_prio;
  switch (prio) {
  case LBM_THREAD_PRIO_LOW:
    freertos_prio = tskIDLE_PRIORITY + 1;
    break;
  case LBM_THREAD_PRIO_NORMAL:
    freertos_prio = tskIDLE_PRIORITY + 2;
    break;
  case LBM_THREAD_PRIO_HIGH:
    freertos_prio = configMAX_PRIORITIES - 2;
    break;
  case LBM_THREAD_PRIO_REALTIME:
    freertos_prio = configMAX_PRIORITIES - 1;
    break;
  default:
    freertos_prio = tskIDLE_PRIORITY + 2;
  }

  thread->handle = xTaskCreateStatic((TaskFunction_t)func,
                                     name ? name : "lbm_thread",
                                     stack_words,
                                     arg,
                                     freertos_prio,
                                     stack,
                                     &thread->task_buffer);
  if (!thread->handle) {
    lbm_free(stack);
    return false;
  }

  return true;
}

void lbm_thread_sleep_us(uint32_t microseconds) {
  TickType_t ticks = (microseconds * configTICK_RATE_HZ) / 1000000;
  if (ticks == 0) ticks = 1;
  vTaskDelay(ticks);
}

void lbm_thread_yield(void) {
  taskYIELD();
}

void lbm_thread_destroy(lbm_thread_t *t) {
  if (t) {
    platform_thread_t *thread = t;

    if (thread->handle) {
      vTaskDelete(thread->handle);
    }

    if (thread->stack) {
      lbm_free(thread->stack);
    }
  }
}
