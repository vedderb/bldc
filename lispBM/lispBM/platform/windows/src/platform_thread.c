/*
    Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

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
#include <stdlib.h>

typedef struct {
  lbm_thread_func_t user_func;
  void *user_arg;
} thread_start_data_t;

static DWORD WINAPI thread_wrapper(LPVOID arg) {
  thread_start_data_t *data = (thread_start_data_t *)arg;
  lbm_thread_func_t func = data->user_func;
  void *user_arg = data->user_arg;
  free(data);

  func(user_arg);
  return 0;
}

bool lbm_thread_create(lbm_thread_t *t,
                       const char *name,
                       lbm_thread_func_t func,
                       void *arg,
                       lbm_thread_prio_t prio,
                       uint32_t stack_size) {
  (void)name;

  platform_thread_t *thread = (platform_thread_t *)t;
  if (!thread) return false;

  thread_start_data_t *start_data = (thread_start_data_t *)malloc(sizeof(thread_start_data_t));
  if (!start_data) return false;

  start_data->user_func = func;
  start_data->user_arg = arg;

  HANDLE h = CreateThread(NULL,
                          (SIZE_T)stack_size,
                          thread_wrapper,
                          start_data,
                          CREATE_SUSPENDED,
                          NULL);
  if (!h) {
    free(start_data);
    return false;
  }

  int win_prio;
  switch (prio) {
  case LBM_THREAD_PRIO_LOW:
    win_prio = THREAD_PRIORITY_BELOW_NORMAL;
    break;
  case LBM_THREAD_PRIO_HIGH:
    win_prio = THREAD_PRIORITY_ABOVE_NORMAL;
    break;
  case LBM_THREAD_PRIO_REALTIME:
    win_prio = THREAD_PRIORITY_TIME_CRITICAL;
    break;
  case LBM_THREAD_PRIO_NORMAL:
  default:
    win_prio = THREAD_PRIORITY_NORMAL;
    break;
  }
  SetThreadPriority(h, win_prio);
  ResumeThread(h);

  thread->handle = h;
  return true;
}

void lbm_thread_sleep_us(uint32_t microseconds) {
  HANDLE timer = CreateWaitableTimer(NULL, TRUE, NULL);
  if (!timer) return;
  LARGE_INTEGER delay;
  delay.QuadPart = -(LONGLONG)microseconds * 10; // 100ns units, negative = relative
  if (SetWaitableTimer(timer, &delay, 0, NULL, NULL, FALSE)) {
    WaitForSingleObject(timer, INFINITE);
  }
  CloseHandle(timer);
}

void lbm_thread_yield(void) {
  SwitchToThread();
}

void lbm_thread_destroy(lbm_thread_t *t) {
  if (!t) return;
  platform_thread_t *thread = (platform_thread_t *)t;
  WaitForSingleObject(thread->handle, INFINITE);
  CloseHandle(thread->handle);
  thread->handle = NULL;
}
