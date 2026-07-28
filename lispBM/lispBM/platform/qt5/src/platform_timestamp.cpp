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

#include "platform_timestamp.h"
#include <QThread>
#include <QElapsedTimer>
#include <QAtomicInteger>

static QAtomicInteger<quint32> timestamp_cache(0);
static QAtomicInteger<int>     timestamp_running(0);
static QElapsedTimer           elapsed_timer;

void lbm_timestamp_cacher(void *v) {
  (void)v;
  timestamp_running.storeRelease(1);
  elapsed_timer.start();
  while (timestamp_running.loadAcquire()) {
    quint32 us = static_cast<quint32>(elapsed_timer.nsecsElapsed() / 1000);
    timestamp_cache.storeRelease(us);
    QThread::usleep(100);
  }
}

void lbm_timestamp_cacher_stop(void) {
  timestamp_running.storeRelease(0);
}

uint32_t lbm_timestamp(void) {
  return static_cast<uint32_t>(timestamp_cache.loadAcquire());
}
