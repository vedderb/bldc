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

#include <QGuiApplication>
#include <QQmlApplicationEngine>
#include <QQmlContext>
#include <QQuickStyle>
#include <QQuickItem>
#include <QQuickWindow>
#include <QKeyEvent>
#include <QFile>
#include <QDebug>

#ifdef Q_OS_ANDROID
#include <QtAndroid>

// Intercept the Android back gesture (left/right edge swipe) so the
// activity moves to the background instead of calling finish().
// finish() destroys the EGL surface while the render thread is still
// active, producing "bad color buffer handle" crashes.
class AndroidBackHandler : public QObject {
protected:
  bool eventFilter(QObject *obj, QEvent *event) override {
    if (event->type() == QEvent::KeyPress) {
      if (static_cast<QKeyEvent *>(event)->key() == Qt::Key_Back) {
        QtAndroid::androidActivity().callMethod<jboolean>(
            "moveTaskToBack", "(Z)Z", true);
        return true;
      }
    }
    return QObject::eventFilter(obj, event);
  }
};
#endif

int main(int argc, char *argv[]) {
  QGuiApplication app(argc, argv);
  app.setApplicationName("LispBM Sim");

#ifdef Q_OS_ANDROID
  static AndroidBackHandler backHandler;
  app.installEventFilter(&backHandler);
#endif

#ifdef Q_OS_ANDROID
  QQuickStyle::setStyle("Material");
#else
  QQuickStyle::setStyle("Fusion");
#endif

  LbmEditorBridge bridge;

  QQmlApplicationEngine engine;
  engine.rootContext()->setContextProperty("bridge", &bridge);

  QObject::connect(
    &engine, &QQmlApplicationEngine::objectCreated,
    &app, [&bridge](QObject *obj, const QUrl &) {
      if (!obj) return;

      // Keep the GL context and scene graph alive across Android
      // suspend/resume so the emulator's color buffer handles stay valid
      // when the user swipes away and returns.
      if (QQuickWindow *w = qobject_cast<QQuickWindow *>(obj)) {
        w->setPersistentOpenGLContext(true);
        w->setPersistentSceneGraph(true);
      }

      QQuickItem *lbmArea = obj->findChild<QQuickItem *>("lbmArea");
      if (!lbmArea) {
        qWarning("lbm_sim: lbmArea item not found in QML");
        return;
      }
      bridge.setRootItem(lbmArea, qmlEngine(lbmArea));

      QFile f(":/scenario.lbm");
      if (!f.open(QIODevice::ReadOnly)) {
        qWarning("lbm_sim: :/scenario.lbm not found in resources");
        return;
      }
      bridge.evaluate(QString::fromUtf8(f.readAll()));
    });

  engine.load(QUrl(QStringLiteral("qrc:/main.qml")));

  return app.exec();
}
