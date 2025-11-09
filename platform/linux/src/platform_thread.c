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

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#include "platform_thread.h"
#include "lbm_memory.h"
#include <time.h>
#include <sched.h>
#include <errno.h>
#include <string.h>

typedef struct {
  lbm_thread_func_t user_func;
  void *user_arg;
} thread_start_data_t;

static void *thread_wrapper(void *arg) {
  thread_start_data_t *data = (thread_start_data_t *)arg;
  lbm_thread_func_t func = data->user_func;
  void *user_arg = data->user_arg;
  lbm_free(data);

  func(user_arg);
  return NULL;
}

bool lbm_thread_create(lbm_thread_t *t,
                       const char *name,
                       lbm_thread_func_t func,
                       void *arg,
                       lbm_thread_prio_t prio,
                       uint32_t stack_size) {

  platform_thread_t *thread = (platform_thread_t *)t;
  if (!thread) return false;

  thread_start_data_t *start_data = (thread_start_data_t *)lbm_malloc(sizeof(thread_start_data_t));
  if (!start_data) {
    return false;
  }

  start_data->user_func = func;
  start_data->user_arg = arg;

  pthread_attr_t attr;
  pthread_attr_init(&attr);

  if (stack_size) pthread_attr_setstacksize(&attr, stack_size);

  if (prio == LBM_THREAD_PRIO_REALTIME) {
    struct sched_param param;
    param.sched_priority = sched_get_priority_max(SCHED_FIFO);
    pthread_attr_setschedpolicy(&attr, SCHED_FIFO);
    pthread_attr_setschedparam(&attr, &param);
  }

  int result = pthread_create(&thread->handle, &attr, thread_wrapper, start_data);
  pthread_attr_destroy(&attr);

  if (result != 0) {
    lbm_free(start_data);
    return false;
  }
  // np apparently means "non-portable"
  // also requires the __GNU_SOURCE define.
  if (name) pthread_setname_np(thread->handle, name);

  return true;
}

void lbm_thread_sleep_us(uint32_t microseconds) {
  struct timespec ts;
  ts.tv_sec = (__time_t)(microseconds / 1000000);
  ts.tv_nsec = (__syscall_slong_t)((microseconds % 1000000) * 1000);
  nanosleep(&ts, NULL);
}

void lbm_thread_yield(void) {
  sched_yield();
}

void lbm_thread_destroy(lbm_thread_t *thread) {
  if (!thread) return;
  pthread_join(thread->handle, NULL);
}
