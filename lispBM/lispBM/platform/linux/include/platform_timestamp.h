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

#ifndef PLATFORM_TIMESTAMP_H_
#define PLATFORM_TIMESTAMP_H_

#include <stdint.h>
#include <stdatomic.h>

// Only on OS where timestamp is expensive
extern void lbm_timestamp_cacher(void *v);
extern void lbm_timestamp_cacher_stop(void);

// timestamp interface
extern atomic_uint_least32_t lbm_timestamp_cache;

static inline uint32_t lbm_timestamp(void) {
  return (uint32_t)atomic_load_explicit(&lbm_timestamp_cache, memory_order_relaxed);
}

#endif
