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

#include "platform_mutex.h"
#include <QMutex>

bool lbm_mutex_init(lbm_mutex_t *m) {
  m->handle = new QMutex();
  return m->handle != nullptr;
}

void lbm_mutex_lock(lbm_mutex_t *m) {
  static_cast<QMutex *>(m->handle)->lock();
}

void lbm_mutex_unlock(lbm_mutex_t *m) {
  static_cast<QMutex *>(m->handle)->unlock();
}
