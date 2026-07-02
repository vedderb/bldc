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
#include <QUrl>

int main(int argc, char *argv[]) {
  QGuiApplication app(argc, argv);
  app.setApplicationName("LispBM Editor");

  QQuickStyle::setStyle("Fusion");

  LbmEditorBridge bridge;

  QQmlApplicationEngine engine;
  engine.rootContext()->setContextProperty("bridge", &bridge);

  // Once the QML root object is created, find the named lbmArea item
  // and hand it to the bridge so the LBM runtime can populate it.
  QObject::connect(
    &engine, &QQmlApplicationEngine::objectCreated,
    &app, [&bridge](QObject *obj, const QUrl &) {
      if (!obj) return;
      QQuickItem *lbmArea = obj->findChild<QQuickItem *>("lbmArea");
      if (lbmArea)
        bridge.setRootItem(lbmArea, qmlEngine(lbmArea));
    });

  engine.load(QUrl(QStringLiteral("qrc:/main.qml")));

  return app.exec();
}
