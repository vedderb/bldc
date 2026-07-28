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

#include "LbmEditorBridge.h"

extern "C" {
#include "lispbm.h"
}

// Custom print extension: routes lbm_print calls to the output panel.
static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  char buf[256];
  for (lbm_uint i = 0; i < argn; i++) {
    lbm_print_value(buf, sizeof(buf), args[i]);
    lbm_printf_callback("%s%s", buf, i < argn - 1 ? " " : "");
  }
  lbm_printf_callback("\n");
  return ENC_SYM_TRUE;
}

LbmEditorBridge::LbmEditorBridge(QObject *parent)
  : QObject(parent)
  , m_lbm(new QLispBM(this)) {
  connect(m_lbm, &QLispBM::output,       this, &LbmEditorBridge::onOutput);
  connect(m_lbm, &QLispBM::evalFinished, this, &LbmEditorBridge::onEvalFinished);
  connect(m_lbm, &QLispBM::evalFailed,   this, &LbmEditorBridge::onEvalFailed);
}

void LbmEditorBridge::setRootItem(QQuickItem *lbmArea, QQmlEngine *engine) {
  m_lbm->setRootItem(lbmArea, engine);
  m_lbm->init();
  m_lbm->addExtension("print", ext_print);
  m_lbm->start();
}

void LbmEditorBridge::evaluate(const QString &code) {
  QString trimmed = code.trimmed();
  if (!trimmed.isEmpty())
    m_lbm->evalProgram(trimmed);
}

void LbmEditorBridge::clearOutput() {
  m_outputText.clear();
  emit outputTextChanged();
  emit outputCleared();
}

void LbmEditorBridge::onOutput(const QString &text) {
  appendOutput(text);
}

void LbmEditorBridge::onEvalFinished(int cid, const QString &result) {
  (void)cid;
  appendOutput(QString("> %1\n").arg(result));
}

void LbmEditorBridge::onEvalFailed(int cid, const QString &error) {
  (void)cid;
  appendOutput(QString("ERROR: %1\n").arg(error));
}

void LbmEditorBridge::appendOutput(const QString &text) {
  m_outputText += text;
  emit outputTextChanged();
  emit outputAppended(text);
}
