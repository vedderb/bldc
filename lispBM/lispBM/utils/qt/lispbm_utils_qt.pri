# Qt Quick / QML backend for LispBM UI extensions.
#
# Usage: include(path/to/utils/qt/lispbm_utils_qt.pri)
#
# Host application requirements:
#   - QApplication (widgets required for QCustomPlot)
#   - Qt Quick Controls style should be set before the engine is created, e.g.:
#       QQuickStyle::setStyle("Fusion");
#   - Call QLispBM::setRootItem(rootItem, engine) before init().

QT += quick quickcontrols2 widgets printsupport

INCLUDEPATH += $$PWD

HEADERS += \
    $$PWD/QLbmQuickDisplayItem.h \
    $$PWD/QLispBM.h \
    $$PWD/qtquick_extensions.h \
    $$PWD/QLbmValue.h \
    $$PWD/QLbmPlotItem.h \
    $$PWD/qcustomplot.h

SOURCES += \
    $$PWD/src/QLbmQuickDisplayItem.cpp \
    $$PWD/src/QLispBM.cpp \
    $$PWD/src/qtquick_extensions.cpp \
    $$PWD/src/QLbmValue.cpp \
    $$PWD/src/QLbmPlotItem.cpp \
    $$PWD/qcustomplot.cpp
