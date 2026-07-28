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

#include "repl_qt.h"
#include "qtquick_extensions.h"

#include <QApplication>
#include <QQmlApplicationEngine>
#include <QQuickItem>
#include <QQuickStyle>
#include <QQuickWindow>

extern "C" {
#include "lispbm.h"
#include "lbm_flat_value.h"
}

static bool repl_qt_send_event(const QLbmValue &value) {
  if (value.isUnboxed())
    return lbm_event_unboxed(value.unboxed());
  lbm_flat_value_t fv;
  if (!value.flatten(&fv)) return false;
  if (!lbm_event(&fv)) { lbm_free(fv.buf); return false; }
  return true;
}

extern "C" void repl_qt_run(int argc, char **argv) {
    QApplication app(argc, argv);
    QQuickStyle::setStyle("Fusion");

    QQmlApplicationEngine engine;
    engine.loadData(
        "import QtQuick 2.15\n"
        "import QtQuick.Window 2.15\n"
        "Window {\n"
        "    visible: true\n"
        "    width: 800\n"
        "    height: 600\n"
        "    title: \"LispBM\"\n"
        "}\n"
    );

    if (engine.rootObjects().isEmpty()) {
        return;
    }

    QQuickWindow *window = qobject_cast<QQuickWindow *>(engine.rootObjects().first());
    if (window) {
        lbm_qtquick_set_event_sender(repl_qt_send_event);
        lbm_qtquick_extensions_set_root(window->contentItem(), &engine);
        lbm_qtquick_extensions_init();
    }

    app.exec();
}
