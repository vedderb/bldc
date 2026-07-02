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
#include <QThread>

// Wrapper around C thread function
class LbmThread : public QThread {
public:
  LbmThread(lbm_thread_func_t func, void *arg)
    : m_func(func), m_arg(arg) {}
protected:
  void run() override {
    m_func(m_arg);
  }
private:
  lbm_thread_func_t m_func;
  void             *m_arg;
};

bool lbm_thread_create(lbm_thread_t *t,
                       const char *name,
                       lbm_thread_func_t func,
                       void *arg,
                       lbm_thread_prio_t prio,
                       uint32_t stack_size) {
  (void)stack_size;
  LbmThread *thread = new LbmThread(func, arg);
  if (!thread) return false;

  if (name) thread->setObjectName(QString::fromUtf8(name));

  t->handle = thread;
  thread->start();

  switch (prio) {
  case LBM_THREAD_PRIO_LOW:
    thread->setPriority(QThread::LowPriority);
    break;
  case LBM_THREAD_PRIO_HIGH:
    thread->setPriority(QThread::HighPriority);
    break;
  case LBM_THREAD_PRIO_REALTIME:
    thread->setPriority(QThread::TimeCriticalPriority);
    break;
  case LBM_THREAD_PRIO_NORMAL:
  default:
    thread->setPriority(QThread::NormalPriority);
    break;
  }

  return true;
}

void lbm_thread_sleep_us(uint32_t microseconds) {
  QThread::usleep(microseconds);
}

void lbm_thread_yield(void) {
  QThread::yieldCurrentThread();
}

void lbm_thread_destroy(lbm_thread_t *t) {
  LbmThread *thread = static_cast<LbmThread *>(t->handle);
  if (thread) {
    thread->wait();
    delete thread;
    t->handle = nullptr;
  }
}
