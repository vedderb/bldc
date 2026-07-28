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

#include "QLispBM.h"

#include <QDebug>
#include <QThread>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "lbm_flat_value.h"
#include "platform_timestamp.h"

QLispBM *QLispBM::s_instance = nullptr;

QLispBM::QLispBM(QObject *parent)
  : QObject(parent) {
  Q_ASSERT_X(s_instance == nullptr, "QLispBM",
             "QLispBM is a singleton — the runtime does not support multiple instances");
  s_instance = this;
}

QLispBM::~QLispBM() {
  terminate();
  s_instance = nullptr;
}

void QLispBM::setRootItem(QQuickItem *root, QQmlEngine *engine) {
  lbm_qtquick_extensions_set_root(root, engine);
}

bool QLispBM::init(const QLispBMConfig *config) {
  if (m_initialized) return false;

  if (config)
    m_config = *config;

  int memWords    = LBM_MEMORY_SIZE_BLOCKS_TO_WORDS(m_config.memoryBlocks);
  int bitmapWords = LBM_MEMORY_BITMAP_SIZE(m_config.memoryBlocks);

  m_heap       = new lbm_cons_t[m_config.heapCells];
  m_memory     = new lbm_uint[memWords];
  m_bitmap     = new lbm_uint[bitmapWords];
  m_extensions = new lbm_extension_t[m_config.maxExtensions];
  m_image      = new uint32_t[m_config.imageWords]();

  if (!lbm_init(m_heap, m_config.heapCells,
                m_memory, memWords,
                m_bitmap, bitmapWords,
                m_config.gcStackSize,
                m_config.printStackSize,
                m_extensions, m_config.maxExtensions)) {
    qWarning("QLispBM: lbm_init failed"); terminate(); return false;
  }

  if (!lbm_eval_init_events(m_config.maxEvents)) {
    qWarning("QLispBM: lbm_eval_init_events failed"); terminate(); return false;
  }

  lbm_set_printf_callback((int (*)(const char*, ...))printCallback);
  lbm_set_ctx_done_callback(doneCallback);
  lbm_set_critical_error_callback(criticalCallback);
  lbm_set_usleep_callback(sleepCallback);

  lbm_image_init(m_image, m_config.imageWords, imageWriteCallback);
  if (!lbm_image_exists())
    lbm_image_create((char*)"qtquick");

  if (!lbm_image_boot()) {
    qWarning("QLispBM: lbm_image_boot failed"); terminate(); return false;
  }

  lbm_add_eval_symbols();

  if (m_config.extensions & QLispBMConfig::ExtArray)   lbm_array_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtCrypto)  lbm_crypto_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtDisplay) lbm_display_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtDsp)     lbm_dsp_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtEcc)     lbm_ecc_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtMath)    lbm_math_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtMutex)   lbm_mutex_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtRandom)  lbm_random_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtRuntime) lbm_runtime_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtSet)     lbm_set_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtString)  lbm_string_extensions_init();
  if (m_config.extensions & QLispBMConfig::ExtTtf)     lbm_ttf_extensions_init();

  lbm_dyn_lib_init();
  lbm_set_dynamic_load_callback(dynLoadCallback);

  lbm_qtquick_extensions_init();

  m_initialized = true;
  emit initializedChanged(true);
  return true;
}

bool QLispBM::start() {
  if (!m_initialized || m_running) return false;

  if (!lbm_thread_create(&m_timestampThread,
                         "lbm_timestamp",
                         lbm_timestamp_cacher,
                         nullptr,
                         LBM_THREAD_PRIO_LOW,
                         0)) {
    qWarning("QLispBM: timestamp thread creation failed"); return false;
  }

  if (!lbm_thread_create(&m_evalThread,
                         "lbm_eval",
                         evalThreadFunc,
                         nullptr,
                         LBM_THREAD_PRIO_NORMAL,
                         0)) {
    qWarning("QLispBM: eval thread creation failed"); return false;
  }

  m_running = true;
  emit runningChanged(true);
  return true;
}

void QLispBM::stop() {
  if (!m_running) return;
  lbm_kill_eval();
  lbm_thread_destroy(&m_evalThread);
  lbm_timestamp_cacher_stop();
  lbm_thread_destroy(&m_timestampThread);
  m_running = false;
  emit runningChanged(false);
}

void QLispBM::terminate() {
  stop();

  if (m_initialized) {
    lbm_image_save_constant_heap_ix();
    lbm_image_save_global_env();
  }

  delete[] m_heap;       m_heap       = nullptr;
  delete[] m_memory;     m_memory     = nullptr;
  delete[] m_bitmap;     m_bitmap     = nullptr;
  delete[] m_extensions; m_extensions = nullptr;
  delete[] m_image;      m_image      = nullptr;

  m_extensionNames.clear();
  m_initialized = false;
  emit initializedChanged(false);
}

bool QLispBM::addExtension(const QString &name, extension_fptr fn) {
  if (!m_initialized) return false;

  if (!m_extensionNames.contains(name))
    m_extensionNames[name] = name.toUtf8();
  char *namePtr = m_extensionNames[name].data();

  if (!m_running) {
    return lbm_add_extension(namePtr, fn);
  }
  lbm_pause_eval();
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED)
    QThread::msleep(1);
  bool ok = lbm_add_extension(namePtr, fn);
  lbm_continue_eval();
  return ok;
}

QHash<QString, QLbmValue> QLispBM::environment() {
  QHash<QString, QLbmValue> env;
  if (!m_initialized || !m_running) return env;

  lbm_pause_eval();
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED)
    QThread::msleep(1);

  lbm_value *glob_env = lbm_get_global_env();
  for (int i = 0; i < GLOBAL_ENV_ROOTS; i++) {
    lbm_value curr = glob_env[i];
    while (lbm_is_cons(curr)) {
      lbm_value binding = lbm_car(curr);
      QString name = QLbmValue::fromLbmValue(lbm_car(binding)).asSymbol();
      env.insert(name, QLbmValue::fromLbmValue(lbm_cdr(binding)));
      curr = lbm_cdr(curr);
    }
  }
  lbm_continue_eval();
  return env;
}

void QLispBM::eval(const QString &code) {
  if (!m_running) return;

  QByteArray ba = code.toUtf8();
  char *buf = (char *)malloc(ba.size() + 1);
  if (!buf) return;
  memcpy(buf, ba.constData(), ba.size() + 1);

  auto *state = new lbm_string_channel_state_t;
  auto *chan  = new lbm_char_channel_t;
  lbm_create_string_char_channel(state, chan, buf);

  lbm_pause_eval_with_gc(20);
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED)
    QThread::msleep(1);
  lbm_cid cid = lbm_load_and_eval_expression(chan);
  lbm_continue_eval();

  if (cid < 0) { free(buf); delete state; delete chan; return; }

  QMutexLocker locker(&m_bufferMutex);
  m_pendingBuffers.insert(cid, new PendingEval{buf, state, chan});
}

void QLispBM::evalProgram(const QString &code) {
  if (!m_running) return;

  QByteArray ba = code.toUtf8();
  char *buf = (char *)malloc(ba.size() + 1);
  if (!buf) return;
  memcpy(buf, ba.constData(), ba.size() + 1);

  auto *state = new lbm_string_channel_state_t;
  auto *chan  = new lbm_char_channel_t;
  lbm_create_string_char_channel(state, chan, buf);

  lbm_pause_eval_with_gc(20);
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED)
    QThread::msleep(1);
  lbm_cid cid = lbm_load_and_eval_program(chan, nullptr);
  lbm_continue_eval();

  if (cid < 0) { free(buf); delete state; delete chan; return; }

  QMutexLocker locker(&m_bufferMutex);
  m_pendingBuffers.insert(cid, new PendingEval{buf, state, chan});
}

bool QLispBM::sendEvent(const QLbmValue &value) {
  if (!m_running) return false;

  if (value.isUnboxed())
    return lbm_event_unboxed(value.unboxed());

  lbm_flat_value_t fv;
  if (!value.flatten(&fv)) return false;
  if (!lbm_event(&fv)) { lbm_free(fv.buf); return false; }
  return true;
}

int QLispBM::printCallback(const char *fmt, ...) {
  char buf[1024];
  va_list args;
  va_start(args, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, args);
  va_end(args);
  emit s_instance->output(QString::fromUtf8(buf));
  return n;
}

void QLispBM::doneCallback(eval_context_t *ctx) {
  char buf[1024];
  lbm_print_value(buf, sizeof(buf), ctx->r);
  QString result = QString::fromUtf8(buf);
  int     cid    = (int)ctx->id;

  if (lbm_is_error(ctx->r))
    emit s_instance->evalFailed(cid, result);
  else
    emit s_instance->evalFinished(cid, result);

  QMutexLocker locker(&s_instance->m_bufferMutex);
  PendingEval *pending = s_instance->m_pendingBuffers.take(cid);
  if (pending) {
    free(pending->buf);
    delete pending->state;
    delete pending->chan;
    delete pending;
  }
}

void QLispBM::criticalCallback(void) {
  emit s_instance->evalFailed(-1, QStringLiteral("critical error: GC stack overflow"));
  s_instance->stop();
}

void QLispBM::sleepCallback(uint32_t us) {
  lbm_thread_sleep_us(us);
}

bool QLispBM::dynLoadCallback(const char *str, const char **code) {
  return lbm_dyn_lib_find(str, code);
}

bool QLispBM::imageWriteCallback(uint32_t data, int32_t index, bool const_heap) {
  (void)const_heap;
  s_instance->m_image[index] = data;
  return true;
}

void QLispBM::evalThreadFunc(void *arg) {
  (void)arg;
  lbm_run_eval();
}
