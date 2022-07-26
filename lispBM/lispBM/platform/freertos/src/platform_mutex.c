/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

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


#include "platform_mutex.h"

bool mutex_init(mutex_t *m) {
  *m = xSemaphoreCreateMutex();
  if (*m != NULL)
    return true;
  return false;
}

void mutex_lock(mutex_t *m) {
  xSemaphoreTake(*m, portMAX_DELAY);
}

void mutex_unlock(mutex_t *m) {
  xSemaphoreGive(*m);
}
