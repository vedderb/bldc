QT += core quick quickcontrols2 widgets

CONFIG += c++17

TARGET   = lbm_editor_quick
TEMPLATE = app

include(../../platform/qt5/lispbm_platform.pri)
include(../../utils/qt/lispbm_utils_qt.pri)

SOURCES += \
    main.cpp \
    LbmEditorBridge.cpp

HEADERS += \
    LbmEditorBridge.h

RESOURCES += resources.qrc
