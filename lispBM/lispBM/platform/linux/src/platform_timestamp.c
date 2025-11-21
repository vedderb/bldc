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

#define _POSIX_C_SOURCE 200809L // nanosleep?
#include "platform_timestamp.h"
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdatomic.h>

static atomic_uint_least32_t timestamp_cache = ATOMIC_VAR_INIT(0);

void *lbm_timestamp_cacher(void *v) {
  (void) v;
  while(true) {
    struct timeval tv;
    gettimeofday(&tv,NULL);
    atomic_store(&timestamp_cache, (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec));
    long us = 100; // sleep 100 us between cache updates.
    struct timespec s;
    struct timespec r;
    s.tv_sec = 0;
    s.tv_nsec = (long)us * 1000;
    nanosleep(&s, &r);
  }
}

uint32_t lbm_timestamp(void) {
  return (uint32_t)atomic_load(&timestamp_cache);
}
