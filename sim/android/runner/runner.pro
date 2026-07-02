QT += core quick quickcontrols2
android: QT += androidextras

CONFIG += c++17

TARGET   = lbm_sim_runner
TEMPLATE = app

include(../../../platform/qt5/lispbm_platform.pri)
include(../../../utils/qt/lispbm_utils_qt.pri)

# Reuse the editor bridge — it already owns QLispBM and forwards output.
INCLUDEPATH += ../../../examples/qtquick-lbm-editor
SOURCES     += ../../../examples/qtquick-lbm-editor/LbmEditorBridge.cpp
HEADERS     += ../../../examples/qtquick-lbm-editor/LbmEditorBridge.h

SOURCES += main.cpp

RESOURCES += resources.qrc

# Scenario QRC — varies per simulation run.
# Override at build time: qmake SCENARIO_QRC=/absolute/path/to/scenario.qrc
isEmpty(SCENARIO_QRC): SCENARIO_QRC = $$PWD/../scenarios/hello_widgets/scenario.qrc
RESOURCES += $$SCENARIO_QRC

# Widgets module is pulled in by lispbm_platform.pri but is not needed
# for a Qt Quick application on Android.
android: QT -= widgets

android {
    ANDROID_MIN_SDK_VERSION    = 23
    ANDROID_TARGET_SDK_VERSION = 30
}
