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

#ifndef PLATFORM_MUTEX_H_
#define PLATFORM_MUTEX_H_

#include <stdbool.h>

typedef struct {
  void *handle;
} lbm_mutex_t;

#ifdef __cplusplus
extern "C" {
#endif

bool lbm_mutex_init(lbm_mutex_t *m);
void lbm_mutex_lock(lbm_mutex_t *m);
void lbm_mutex_unlock(lbm_mutex_t *m);

#ifdef __cplusplus
}
#endif

#endif
