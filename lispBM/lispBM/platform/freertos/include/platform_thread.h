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

#ifndef PLATFORM_THREAD_H_
#define PLATFORM_THREAD_H_

#include <FreeRTOS.h>
#include <task.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct {
  TaskHandle_t handle;
  StackType_t *stack;
  StaticTask_t task_buffer;
  uint32_t stack_size;
} platform_thread_t;

typedef platform_thread_t lbm_thread_t;

typedef void (*lbm_thread_func_t)(void *arg);

typedef enum {
  LBM_THREAD_PRIO_LOW = 0,
  LBM_THREAD_PRIO_NORMAL = 1,
  LBM_THREAD_PRIO_HIGH = 2,
  LBM_THREAD_PRIO_REALTIME = 3
} lbm_thread_prio_t;

extern bool lbm_thread_create(lbm_thread_t *t,
                               const char *name,
                               lbm_thread_func_t func,
                               void *arg,
                               lbm_thread_prio_t prio,
                               uint32_t stack_size);
extern void lbm_thread_sleep_us(uint32_t microseconds);
extern void lbm_thread_yield(void);
extern void lbm_thread_destroy(lbm_thread_t *t);

#endif
