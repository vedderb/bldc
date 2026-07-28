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

#ifndef QLISPBM_H_
#define QLISPBM_H_

#include <QObject>
#include <QHash>
#include <QMutex>
#include <QString>
#include <QQuickItem>
#include <QQmlEngine>

#include "QLbmValue.h"
#include "qtquick_extensions.h"

#include "lispbm.h"
#include "lbm_image.h"
#include "platform_thread.h"
#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"
#include "extensions/random_extensions.h"
#include "extensions/set_extensions.h"
#include "extensions/lbm_dyn_lib.h"
#include "extensions/crypto_extensions.h"
#include "extensions/ecc_extensions.h"
#include "extensions/dsp_extensions.h"
#include "extensions/mutex_extensions.h"
#include "extensions/ttf_extensions.h"
#include "extensions/display_extensions.h"

struct QLispBMConfig {
  int heapCells      = 16384;
  int memoryBlocks   = 131072; // 8MB
  int gcStackSize    = 256;
  int printStackSize = 256;
  int maxExtensions  = 256;
  int imageWords     = 32768;
  int maxEvents      = 20;

  enum Extension {
    ExtArray   = 1 << 0,
    ExtString  = 1 << 1,
    ExtMath    = 1 << 2,
    ExtRuntime = 1 << 3,
    ExtRandom  = 1 << 4,
    ExtSet     = 1 << 5,
    ExtMutex   = 1 << 6,
    ExtCrypto  = 1 << 7,
    ExtEcc     = 1 << 8,
    ExtDisplay = 1 << 9,
    ExtDsp     = 1 << 10,
    ExtTtf     = 1 << 11,
    ExtAll     = 0xFFFF
  };

  uint32_t extensions = ExtAll;
};

// There can be only one QLispBM object.
class QLispBM : public QObject {
  Q_OBJECT
  Q_DISABLE_COPY(QLispBM)

  Q_PROPERTY(bool initialized READ isInitialized NOTIFY initializedChanged)
  Q_PROPERTY(bool running     READ isRunning     NOTIFY runningChanged)

public:
  explicit QLispBM(QObject *parent = nullptr);
  ~QLispBM();

  static QLispBM *instance() { return s_instance; }

  bool isInitialized() const { return m_initialized; }
  bool isRunning()     const { return m_running; }

  bool init(const QLispBMConfig *config = nullptr);
  bool start();
  void stop();
  void terminate();

  // Set the root QQuickItem and QQmlEngine before calling init().
  // A ColumnLayout filling the root item is created automatically.
  void setRootItem(QQuickItem *root, QQmlEngine *engine);

  bool addExtension(const QString &name, extension_fptr fn);
  QHash<QString, QLbmValue> environment();

public slots:
  void eval(const QString &code);
  void evalProgram(const QString &code);
  bool sendEvent(const QLbmValue &value);

signals:
  void output(const QString &text);
  void evalFinished(int cid, const QString &result);
  void evalFailed(int cid, const QString &error);
  void initializedChanged(bool initialized);
  void runningChanged(bool running);

private:
  static QLispBM *s_instance;

  static int  printCallback(const char *fmt, ...);
  static void doneCallback(eval_context_t *ctx);
  static void criticalCallback(void);
  static void sleepCallback(uint32_t us);
  static bool imageWriteCallback(uint32_t data, int32_t index, bool const_heap);
  static bool dynLoadCallback(const char *str, const char **code);
  static void evalThreadFunc(void *arg);

  QLispBMConfig     m_config;
  bool                   m_initialized = false;
  bool                   m_running     = false;

  lbm_cons_t            *m_heap       = nullptr;
  lbm_uint              *m_memory     = nullptr;
  lbm_uint              *m_bitmap     = nullptr;
  lbm_extension_t       *m_extensions = nullptr;
  uint32_t              *m_image      = nullptr;

  lbm_thread_t           m_evalThread;
  lbm_thread_t           m_timestampThread;

  struct PendingEval {
    char                       *buf;
    lbm_string_channel_state_t *state;
    lbm_char_channel_t         *chan;
  };

  QMutex                    m_bufferMutex;
  QHash<int, PendingEval *> m_pendingBuffers;
  QHash<QString, QByteArray> m_extensionNames;
};

#endif
