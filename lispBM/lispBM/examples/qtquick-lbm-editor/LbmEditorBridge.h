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

#ifndef LBMEDITORBRIDGE_H_
#define LBMEDITORBRIDGE_H_

#include <QObject>
#include <QString>
#include <QQuickItem>
#include <QQmlEngine>

#include "QLispBM.h"

// Exposed to QML as the context property "bridge".
// Owns the LispBM runtime and mediates between QML and it.
class LbmEditorBridge : public QObject {
  Q_OBJECT
  Q_PROPERTY(QString outputText READ outputText NOTIFY outputTextChanged)

public:
  explicit LbmEditorBridge(QObject *parent = nullptr);

  QString outputText() const { return m_outputText; }

  // Call this once the QML lbmArea item is available (after engine loads).
  // Triggers init + start of the LBM runtime.
  void setRootItem(QQuickItem *lbmArea, QQmlEngine *engine);

  Q_INVOKABLE void evaluate(const QString &code);
  Q_INVOKABLE void clearOutput();

signals:
  void outputTextChanged();        // full text (for property reads)
  void outputAppended(const QString &text);  // new chunk to append
  void outputCleared();            // text area should be cleared

private slots:
  void onOutput(const QString &text);
  void onEvalFinished(int cid, const QString &result);
  void onEvalFailed(int cid, const QString &error);

private:
  void appendOutput(const QString &text);

  QLispBM *m_lbm;
  QString  m_outputText;
};

#endif
