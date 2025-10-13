import Vedder.vesc.vescinterface 1.0;import "qrc:/mobile";import QtQuick 2.7
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import Qt.labs.settings 1.0 as QSettings
import Vedder.vesc.vescinterface 1.0
import Vedder.vesc.bleuart 1.0
import Vedder.vesc.commands 1.0
import Vedder.vesc.configparams 1.0
import Vedder.vesc.utility 1.0

Item {
    id: mainItem
    anchors.fill: parent
    //anchors.margins: 10    //fix this

    property BleUart mBle: VescIf.bleDevice()
    property Commands mCommands: VescIf.commands()
    property var fwVersion: mCommands.getFwVersion()
    property ConfigParams mMcConf: VescIf.mcConfig()
    property ConfigParams mAppConf: VescIf.appConfig()
    property ConfigParams mInfoConf: VescIf.infoConfig()
    property bool isHorizontal: width > height

    property bool scheduleConfWrite: false
    property bool fixedThrottle: false
    property var parentTabBar: parent.tabBarItem
        
    Component.onCompleted: {
        parentTabBar.visible = true
        parentTabBar.enabled = true
    }
    
    ColumnLayout {
        anchors.fill: parent
        anchors.topMargin: parentTabBar ? parentTabBar.height / 2 : 0
        TabBar {
            id: tabBar
            //parent: parentTabBar
            //anchors.fill: parent

            currentIndex: swipeView.currentIndex
            Layout.fillWidth: true
            implicitWidth: 0
            clip: true

            property int buttons: 4
            property int buttonWidth: 100

            TabButton {
                text: qsTr("Ride")
                width: Math.max(tabBar.buttonWidth,
                                tabBar.width / tabBar.buttons)
            }
            TabButton {
                text: qsTr("Tune")
                width: Math.max(tabBar.buttonWidth,
                                tabBar.width / tabBar.buttons)
            }
            TabButton {
                text: qsTr("Bike CFG")
                width: Math.max(tabBar.buttonWidth,
                                tabBar.width / tabBar.buttons)
            }
            TabButton {
                text: qsTr("Firmware")
                width: Math.max(tabBar.buttonWidth,
                                tabBar.width / tabBar.buttons)
            }
        }

        SwipeView {
            id: swipeView
            currentIndex: tabBar.currentIndex
            Layout.fillHeight: true
            Layout.fillWidth: true

            clip: true

            Page {
                RtDataSetup {
                anchors.fill: parent
                }
            }

            Page {
                background: Rectangle {
                    opacity: 0.0
                }

                ColumnLayout {
                    anchors.topMargin: 5
                    anchors.bottomMargin: 1 //leave space for statusbar?
                    anchors.fill: parent

                    clip: false
                    visible: true
                    spacing: 5

                    TabBar {
                        id: profilesBar
                        currentIndex: profileSwipeView.currentIndex

                        property var lastProfileIndex: currentIndex

                        Layout.fillWidth: true
                        implicitWidth: 0
                        clip: true

                        property int buttons: 3
                        property int buttonWidth: 100

                        TabButton {
                            text: qsTr("Street\nlegal")
                            width: Math.max(
                                       profilesBar.buttonWidth,
                                       profilesBar.width / profilesBar.buttons)
                        }
                        TabButton {
                            text: qsTr("Trail")
                            width: Math.max(
                                       profilesBar.buttonWidth,
                                       profilesBar.width / profilesBar.buttons)
                        }
                        TabButton {
                            text: qsTr("Ludicrous")
                            width: Math.max(
                                       profilesBar.buttonWidth,
                                       profilesBar.width / profilesBar.buttons)
                        }
                    }

                    SwipeView {
                        id: profileSwipeView
                        currentIndex: profilesBar.currentIndex
                        Layout.fillHeight: true
                        Layout.fillWidth: true

                        clip: true


                        // Street Legal
                        Page
                        {
                            ColumnLayout {
                                anchors.topMargin: 5
                                anchors.bottomMargin: 1 //leave space for statusbar?
                                anchors.fill: parent

                                clip: false
                                visible: true
                                spacing: 5

                                ScrollView {
                                    //anchors.fill: parent
                                    clip: true
                                    contentWidth: parent.width
                                    Layout.fillHeight: true

                                    ColumnLayout {
                                        anchors.topMargin: 5
                                        anchors.bottomMargin: 1
                                        anchors.fill: parent

                                        clip: false
                                        visible: true
                                        spacing: 5

                                        Item {
                                            // Spacer
                                            Layout.fillWidth: true
                                            Layout.fillHeight: true
                                        }

                                        GroupBox {
                                            id: streetTorqueBox
                                            //title: qsTr("Throttle Torque")
                                            Layout.fillWidth: true

                                            RowLayout {
                                                // anchors.topMargin: 5
                                                //anchors.bottomMargin: 1
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: streetTorqueSlider
                                                    stepSize: 10
                                                    from: 20
                                                    value: 40
                                                    to: 150 //max phase amps
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: streetTorqueSlider.handle
                                                        visible: streetTorqueSlider.pressed
                                                        text: streetTorqueSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: streetPasBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "PAS\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: streetPasSlider
                                                    stepSize: 1
                                                    from: 0
                                                    value: 20
                                                    to: streetTorqueSlider.value.toFixed(
                                                            1) //% of phase amps
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: streetPasSlider.handle
                                                        visible: streetPasSlider.pressed
                                                        text: streetPasSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: streetPowerBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Power"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: streetPowerSlider
                                                    stepSize: 250
                                                    from: 0
                                                    value: 750
                                                    to: 7500 //could be updated from batt current limit + max voltage
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: streetPowerSlider.handle
                                                        visible: streetPowerSlider.pressed
                                                        text: streetPowerSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: streetRpmBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "RPM"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: streetRpmSlider
                                                    stepSize: 250
                                                    from: 500
                                                    value: 3000
                                                    to: 6500
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: streetRpmSlider.handle
                                                        visible: streetRpmSlider.pressed
                                                        text: streetRpmSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: streetThrottleResponseBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nResponse"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: streetThrottleResponseSlider
                                                    stepSize: 0.1
                                                    from: 3
                                                    value: 0.9
                                                    to: 0.3
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: streetThrottleResponseSlider.handle
                                                        visible: streetThrottleResponseSlider.pressed
                                                        text: streetThrottleResponseSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: streetThrottleExpoBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nLinearity"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: streetThrottleExpoSlider
                                                    stepSize: 1
                                                    from: 0
                                                    value: 90
                                                    to: 100
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: streetThrottleExpoSlider.handle
                                                        visible: streetThrottleExpoSlider.pressed
                                                        text: streetThrottleExpoSlider.value.toFixed(1)
                                                    }
                                                }
                                            }
                                        }
                                        GroupBox {
                                            id: streetFixedThrottleBox
                                            Layout.fillWidth: true
                                            RowLayout {
                                                anchors.fill: parent
                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Fixed Throttle\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                CheckBox {
                                                    id: streetFixedThrottleCheckbox
                                                    checked: false
                                                    Layout.fillWidth: true
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Trail
                        Page {
                            ColumnLayout {
                                anchors.topMargin: 5
                                anchors.bottomMargin: 1 //leave space for statusbar?
                                anchors.fill: parent

                                clip: false
                                visible: true
                                spacing: 5

                                ScrollView {
                                    //anchors.fill: parent
                                    clip: true
                                    contentWidth: parent.width
                                    Layout.fillHeight: true

                                    ColumnLayout {
                                        anchors.topMargin: 5
                                        anchors.bottomMargin: 1
                                        anchors.fill: parent

                                        clip: false
                                        visible: true
                                        spacing: 5

                                        Item {
                                            // Spacer
                                            Layout.fillWidth: true
                                            Layout.fillHeight: true
                                        }

                                        GroupBox {
                                            id: trailTorqueBox
                                            //title: qsTr("Throttle Torque")
                                            Layout.fillWidth: true

                                            RowLayout {
                                                // anchors.topMargin: 5
                                                //anchors.bottomMargin: 1
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: trailTorqueSlider
                                                    stepSize: 10
                                                    from: 20
                                                    value: 100
                                                    to: 150 //max phase amps
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: trailTorqueSlider.handle
                                                        visible: trailTorqueSlider.pressed
                                                        text: trailTorqueSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: trailPasBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "PAS\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: trailPasSlider
                                                    stepSize: 1
                                                    from: 0
                                                    value: 25
                                                    to: trailTorqueSlider.value.toFixed(
                                                            1) //% of phase amps
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: trailPasSlider.handle
                                                        visible: trailPasSlider.pressed
                                                        text: trailPasSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: trailPowerBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Power"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: trailPowerSlider
                                                    stepSize: 250
                                                    from: 0
                                                    value: 2500
                                                    to: 7500 //could be updated from batt current limit + max voltage
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: trailPowerSlider.handle
                                                        visible: trailPowerSlider.pressed
                                                        text: trailPowerSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: trailRpmBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "RPM"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: trailRpmSlider
                                                    stepSize: 250
                                                    from: 500
                                                    value: 5000
                                                    to: 6500
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: trailRpmSlider.handle
                                                        visible: trailRpmSlider.pressed
                                                        text: trailRpmSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: trailThrottleResponseBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nResponse"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: trailThrottleResponseSlider
                                                    stepSize: 0.1
                                                    from: 3
                                                    value: 0.4
                                                    to: 0.3
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: trailThrottleResponseSlider.handle
                                                        visible: trailThrottleResponseSlider.pressed
                                                        text: trailThrottleResponseSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: trailThrottleExpoBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nLinearity"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: trailThrottleExpoSlider
                                                    stepSize: 1
                                                    from: 0
                                                    value: 90
                                                    to: 100
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: trailThrottleExpoSlider.handle
                                                        visible: trailThrottleExpoSlider.pressed
                                                        text: trailThrottleExpoSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }
                                        GroupBox {
                                            id: trailFixedThrottleBox
                                            Layout.fillWidth: true
                                            RowLayout {
                                                anchors.fill: parent
                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Fixed Throttle\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                CheckBox {
                                                    id: trailFixedThrottleCheckbox
                                                    checked: false
                                                    Layout.fillWidth: true
                                                }
                                            }
                                        }
                                    }
                                }


                            }
                        }

                        // Ludicrous
                        Page {
                            ColumnLayout {
                                anchors.topMargin: 5
                                anchors.bottomMargin: 1
                                anchors.fill: parent

                                clip: false
                                visible: true
                                spacing: 5

                                ScrollView {
                                    //anchors.fill: parent
                                    clip: true
                                    contentWidth: parent.width
                                    Layout.fillHeight: true

                                    ColumnLayout {
                                        anchors.topMargin: 5
                                        anchors.bottomMargin: 1
                                        anchors.fill: parent

                                        clip: false
                                        visible: true
                                        spacing: 5

                                        Item {
                                            // Spacer
                                            Layout.fillWidth: true
                                            Layout.fillHeight: true
                                        }

                                        GroupBox {
                                            id: torqueBox
                                            //title: qsTr("Throttle Torque")
                                            Layout.fillWidth: true

                                            RowLayout {
                                                // anchors.topMargin: 5
                                                //anchors.bottomMargin: 1
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: torqueSlider
                                                    stepSize: 10
                                                    from: 20
                                                    value: 150
                                                    to: 150 //max phase amps
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: torqueSlider.handle
                                                        visible: torqueSlider.pressed
                                                        text: torqueSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: pasBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "PAS\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: pasSlider
                                                    stepSize: 1
                                                    from: 0
                                                    value: 35
                                                    to: torqueSlider.value.toFixed(
                                                            1) //% of phase amps
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: pasSlider.handle
                                                        visible: pasSlider.pressed
                                                        text: pasSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: powerBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Power"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: powerSlider
                                                    stepSize: 250
                                                    from: 0
                                                    value: 7500
                                                    to: 7500 //could be updated from batt current limit + max voltage
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: powerSlider.handle
                                                        visible: powerSlider.pressed
                                                        text: powerSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: rpmBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "RPM"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: rpmSlider
                                                    stepSize: 250
                                                    from: 500
                                                    value: 6500
                                                    to: 6500
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: rpmSlider.handle
                                                        visible: rpmSlider.pressed
                                                        text: rpmSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: throttleResponseBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nResponse"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: throttleResponseSlider
                                                    stepSize: 0.1
                                                    from: 3
                                                    value: 0.3
                                                    to: 0.3
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: throttleResponseSlider.handle
                                                        visible: throttleResponseSlider.pressed
                                                        text: throttleResponseSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }

                                        GroupBox {
                                            id: throttleExpoBox
                                            Layout.fillWidth: true

                                            RowLayout {
                                                anchors.fill: parent

                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Throttle\nLinearity"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                Slider {
                                                    id: throttleExpoSlider
                                                    stepSize: 1
                                                    from: 0
                                                    value: 90
                                                    to: 100
                                                    Layout.fillWidth: true

                                                    ToolTip {
                                                        parent: throttleExpoSlider.handle
                                                        visible: throttleExpoSlider.pressed
                                                        text: throttleExpoSlider.value.toFixed(
                                                                  1)
                                                    }
                                                }
                                            }
                                        }
                                        GroupBox {
                                            id: fixedThrottleBox
                                            Layout.fillWidth: true
                                            RowLayout {
                                                anchors.fill: parent
                                                clip: false
                                                visible: true
                                                spacing: 5
                                                Text {
                                                    text: "Fixed Throttle\nAmps"
                                                    color:{color=Utility.getAppHexColor("lightText")}
                                                    horizontalAlignment: Text.AlignHCenter
                                                    Layout.minimumWidth: 100
                                                }
                                                CheckBox {
                                                    id: fixedThrottleCheckbox
                                                    checked: false
                                                    Layout.fillWidth: true
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    RowLayout {
                        id: profilesRow
                        clip: false
                        visible: true
                        spacing: 5

                        Button {
                            text: "Read\nSettings"
                            Layout.columnSpan: 2
                            Layout.preferredWidth: 200
                            Layout.preferredHeight: 80
                            Layout.fillWidth: true
                            onClicked: {
                                tuneWriteSettingsButton.enabled = true
                                bikeWriteSettingsButton.enabled = true

                                mCommands.getMcconf()
                                mCommands.getAppConf()

                                // check for a match in power and phase current to select the profile
                                var useImperial = VescIf.useImperialUnits()
                                var impFact = useImperial ? 39.3701 : 1000.0

                                var power = mMcConf.getParamDouble("l_watt_max")
                                var torque = mMcConf.getParamDouble("l_current_max")
                                var rpm = mMcConf.getParamDouble("l_max_erpm") / 4.0 //(4 pole pairs)
                                var pas = mAppConf.getParamDouble("app_pas_conf.current_scaling") * torque
                                var throttleResponse = mAppConf.getParamDouble("app_adc_conf.ramp_time_pos")
                                var throttleExpo = 100.0 - (mAppConf.getParamDouble("app_adc_conf.throttle_exp")) * -20.0
                                var battCurr = mMcConf.getParamDouble("l_in_current_max")
                                var battCells = mMcConf.getParamInt("si_battery_cells")
                                var battOvervoltage = mMcConf.getParamDouble("l_max_vin")
                                var battUndervoltageStart = mMcConf.getParamDouble("l_battery_cut_start")
                                var battUndervoltageEnd = mMcConf.getParamDouble("l_battery_cut_end")
                                var wheelDiameter = mMcConf.getParamDouble("si_wheel_diameter") * impFact

                                if ((power == streetPowerSlider.value) && (torque == streetTorqueSlider.value)) {
                                    profilesBar.setCurrentIndex(0)

                                    streetTorqueSlider.value = torque
                                    streetPowerSlider.value = power
                                    streetRpmSlider.value = rpm
                                    streetPasSlider.value = pas
                                    streetThrottleResponseSlider.value = throttleResponse
                                    streetThrottleExpoSlider.value = throttleExpo
                                }
                                if ((power == trailPowerSlider.value) && (torque == trailTorqueSlider.value)) {
                                    profilesBar.setCurrentIndex(1)

                                    trailTorqueSlider.value = torque
                                    trailPowerSlider.value = power
                                    trailRpmSlider.value = rpm
                                    trailPasSlider.value = pas
                                    trailThrottleResponseSlider.value = throttleResponse
                                    trailThrottleExpoSlider.value = throttleExpo
                                }
                                if ((power == powerSlider.value) && (torque == torqueSlider.value)) {
                                    profilesBar.setCurrentIndex(2)
                                    torqueSlider.value = torque
                                    powerSlider.value = power
                                    rpmSlider.value = rpm
                                    pasSlider.value = pas
                                    throttleResponseSlider.value = throttleResponse
                                    throttleExpoSlider.value = throttleExpo
                                }
                                battCurrBox.realValue = mMcConf.getParamDouble("l_in_current_max")
                                battCellsBox.realValue = mMcConf.getParamInt("si_battery_cells")
                                battOvervoltageBox.realValue = mMcConf.getParamDouble("l_max_vin")
                                battUndervoltageStartBox.realValue = mMcConf.getParamDouble("l_battery_cut_start")
                                battUndervoltageEndBox.realValue = mMcConf.getParamDouble("l_battery_cut_end")
                                wheelDiameterBox.realValue = mMcConf.getParamDouble("si_wheel_diameter") * impFact
                            }
                        }
                        Button {
                            id: tuneWriteSettingsButton
                            text: "Write\nSettings"
                            enabled: false
                            Layout.columnSpan: 2
                            Layout.preferredWidth: 200
                            Layout.preferredHeight: 80
                            Layout.fillWidth: true
                            onClicked: {
                                var useImperial = VescIf.useImperialUnits()
                                var impFact = useImperial ? 39.3701 : 1000.0

                                if(profilesBar.currentIndex == 0) {
                                    mMcConf.updateParamDouble("l_current_max", streetTorqueSlider.value)
                                    mMcConf.updateParamDouble("l_watt_max", streetPowerSlider.value)
                                    mMcConf.updateParamDouble("l_max_erpm", streetRpmSlider.value * 4.0)
                                    mAppConf.updateParamDouble("app_pas_conf.current_scaling", streetPasSlider.value / streetTorqueSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.ramp_time_pos", streetThrottleResponseSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.throttle_exp", (100.0 - streetThrottleExpoSlider.value) / -20.0)
                                    fixedThrottle = streetFixedThrottleCheckbox.checked
                                }
                                if(profilesBar.currentIndex == 1) {
                                    mMcConf.updateParamDouble("l_current_max", trailTorqueSlider.value)
                                    mMcConf.updateParamDouble("l_watt_max", trailPowerSlider.value)
                                    mMcConf.updateParamDouble("l_max_erpm", trailRpmSlider.value * 4.0)
                                    mAppConf.updateParamDouble("app_pas_conf.current_scaling", trailPasSlider.value / trailTorqueSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.ramp_time_pos", trailThrottleResponseSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.throttle_exp", (100.0 - trailThrottleExpoSlider.value) / -20.0)
                                    fixedThrottle = trailFixedThrottleCheckbox.checked
                                }
                                if(profilesBar.currentIndex == 2) {
                                    mMcConf.updateParamDouble("l_current_max", torqueSlider.value)
                                    mMcConf.updateParamDouble("l_watt_max", powerSlider.value)
                                    mMcConf.updateParamDouble("l_max_erpm", rpmSlider.value * 4.0)
                                    mAppConf.updateParamDouble("app_pas_conf.current_scaling", pasSlider.value / torqueSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.ramp_time_pos", throttleResponseSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.throttle_exp", (100.0 - throttleExpoSlider.value) / -20.0)
                                    fixedThrottle = fixedThrottleCheckbox.checked
                                }
                                mMcConf.updateParamDouble("l_in_current_max", battCurrBox.realValue)
                                mMcConf.updateParamInt("si_battery_cells", battCellsBox.realValue)
                                mMcConf.updateParamDouble("l_max_vin",battOvervoltageBox.realValue)
                                mMcConf.updateParamDouble("l_battery_cut_start",battUndervoltageStartBox.realValue)
                                mMcConf.updateParamDouble("l_battery_cut_end",battUndervoltageEndBox.realValue)
                                mMcConf.updateParamDouble("si_wheel_diameter",wheelDiameterBox.realValue / impFact)
                                scheduleConfWrite = true
                                }
                        }
                    }
                }
            }

            Page {
                background: Rectangle {
                    opacity: 0.0
                }

                ColumnLayout {
                    anchors.topMargin: 5
                    anchors.bottomMargin: 1 //leave space for statusbar?
                    anchors.fill: parent

                    ScrollView {
                        //anchors.fill: parent
                        clip: true
                        contentWidth: parent.width
                        Layout.fillHeight: true
                        //boundsBehavior: StopAtBounds
                        GridLayout {
                            id: grid2
                            anchors.fill: parent
                            columns: 1 //isHorizontal ? 1 : 1
                            columnSpacing: 5
                            rowSpacing: 10

                            GroupBox {
                                id: battSelectorBox
                                title: qsTr("Battery Presets")
                                Layout.fillWidth: true
                                Layout.columnSpan: 1

                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 3

                                    Button {
                                        id: wolfButton
                                        text: "Wolf\n52V"
                                        Layout.columnSpan: 2
                                        Layout.preferredWidth: 200
                                        Layout.preferredHeight: 70
                                        Layout.fillWidth: true

                                        onClicked: {
                                            battCurrBox.realValue = 70.0
                                            battCellsBox.realValue = 14
                                            battOvervoltageBox.realValue = 60.0
                                            battUndervoltageStartBox.realValue = 44.2
                                            battUndervoltageEndBox.realValue = 40.3
                                            //mCommands.getMcconf()
                                            //mCommands.getAppConf()
                                        }
                                    }

                                    Button {
                                        text: "Dire Wolf\n52V"
                                        Layout.columnSpan: 2
                                        Layout.preferredWidth: 200
                                        Layout.preferredHeight: 70
                                        Layout.fillWidth: true
                                        onClicked: {
                                            battCurrBox.realValue = 105.0
                                            battCellsBox.realValue = 14
                                            battOvervoltageBox.realValue = 60.0
                                            battUndervoltageStartBox.realValue = 44.2
                                            battUndervoltageEndBox.realValue = 40.3
                                            //mCommands.getMcconf()
                                            //mCommands.getAppConf()
                                        }
                                    }
                                    Button {
                                        text: "Power Wolf\n72V"
                                        Layout.columnSpan: 2
                                        Layout.preferredWidth: 200
                                        Layout.preferredHeight: 70
                                        Layout.fillWidth: true
                                        onClicked: {
                                            battCurrBox.realValue = 60.0
                                            battCellsBox.realValue = 20
                                            battOvervoltageBox.realValue = 86.0
                                            battUndervoltageStartBox.realValue = 63.1
                                            battUndervoltageEndBox.realValue = 57.6
                                            //mCommands.getMcconf()
                                            //mCommands.getAppConf()
                                        }
                                    }
                                }
                            }

                            GroupBox {
                                id: battCurrentBox
                                // title: qsTr("Battery")
                                Layout.fillWidth: true
                                Layout.columnSpan: 1

                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 0

                                    Text {
                                        text: "Current\nMax"
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        horizontalAlignment: Text.AlignHCenter
                                        Layout.minimumWidth: 100
                                    }

                                    DoubleSpinBox {
                                        id: battCurrBox
                                        Layout.fillWidth: true
                                        decimals: 2
                                        realValue: 105.0
                                        realFrom: 0.0
                                        realTo: 105.0
                                        prefix: "I: "
                                        suffix: " A"
                                    }
                                }
                            }
                            GroupBox {
                                id: batt8Box
                                Layout.fillWidth: true
                                Layout.columnSpan: 1
                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 0
                                    Text {
                                        text: "Cells"
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        horizontalAlignment: Text.AlignHCenter
                                        Layout.minimumWidth: 100
                                    }
                                    DoubleSpinBox {
                                        id: battCellsBox
                                        Layout.fillWidth: true
                                        decimals: 0
                                        realValue: 14.0
                                        realFrom: 13.0
                                        realTo: 20.0
                                        suffix: "s"
                                    }
                                }
                            }
                            GroupBox {
                                id: battBox
                                Layout.fillWidth: true
                                Layout.columnSpan: 1
                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 0

                                    Text {
                                        text: "Overvoltage"
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        horizontalAlignment: Text.AlignHCenter
                                        Layout.minimumWidth: 100
                                    }

                                    DoubleSpinBox {
                                        id: battOvervoltageBox
                                        Layout.fillWidth: true
                                        decimals: 2
                                        realValue: 60.0
                                        realFrom: 0.0
                                        realTo: 86.0
                                        prefix: "V: "
                                        suffix: " V"
                                    }
                                }
                            }
                            GroupBox {
                                id: batt2Box
                                Layout.fillWidth: true
                                Layout.columnSpan: 1
                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 0

                                    Text {
                                        text: "Undervoltage\nCutoff Start"
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        horizontalAlignment: Text.AlignHCenter
                                        Layout.minimumWidth: 100
                                    }

                                    DoubleSpinBox {
                                        id: battUndervoltageStartBox
                                        Layout.fillWidth: true
                                        decimals: 2
                                        realValue: 44.2
                                        realFrom: 0.0
                                        realTo: 86.0
                                        prefix: "V: "
                                        suffix: " V"
                                    }
                                }
                            }
                            GroupBox {
                                id: batt3Box
                                Layout.fillWidth: true
                                Layout.columnSpan: 1
                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 0

                                    Text {
                                        text: "Undervoltage\nCutoff End"
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        horizontalAlignment: Text.AlignHCenter
                                        Layout.minimumWidth: 100
                                    }

                                    DoubleSpinBox {
                                        id: battUndervoltageEndBox
                                        Layout.fillWidth: true
                                        decimals: 2
                                        realValue: 40.3
                                        realFrom: 0.0
                                        realTo: 86.0
                                        prefix: "V: "
                                        suffix: " V"
                                    }
                                }
                            }

                            GroupBox {
                                id: batt4Box
                                title: qsTr("Speedo")
                                Layout.fillWidth: true
                                Layout.columnSpan: 1
                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 0

                                    Text {
                                        text: "Wheel\nDiameter"
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        horizontalAlignment: Text.AlignHCenter
                                        Layout.minimumWidth: 100
                                    }

                                    DoubleSpinBox {
                                        id: wheelDiameterBox
                                        Layout.fillWidth: true
                                        decimals: VescIf.useImperialUnits(
                                                      ) ? 2 : 0
                                        realValue: VescIf.useImperialUnits(
                                                       ) ? 26.0 : 660.0
                                        realFrom: 0.0
                                        realTo: 1500.0
                                        prefix: "⌀: "
                                        suffix: VescIf.useImperialUnits(
                                                    ) ? "inch" : "mm"
                                    }
                                }
                            }

                            GroupBox {
                                id: batt5Box
                                title: qsTr("Misc")
                                Layout.fillWidth: true
                                Layout.columnSpan: 1
                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 0

                                    Text {
                                        text: "Default\nPower Level"
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        horizontalAlignment: Text.AlignHCenter
                                        Layout.minimumWidth: 100
                                    }

                                    DoubleSpinBox {
                                        id: battCurr5Box
                                        Layout.fillWidth: true
                                        decimals: 0
                                        realValue: 1.0
                                        realFrom: 0.0
                                        realTo: 9.0
                                    }
                                }
                            }
                            GroupBox {
                                id: batt6Box
                                title: "Data Logging"
                                Layout.fillWidth: true
                                Layout.columnSpan: 1

                            Switch {
                            id: rtLogEnBox
                            text: "Enable RT Data Logging"
                            anchors.centerIn: parent
                            Layout.fillWidth: true
                            Layout.columnSpan: 2

                            onClicked: {
                                if (checked) {
                                    if (VescIf.openRtLogFile(
                                                rtLogFileText.text)) {
                                        Utility.startGnssForegroundService()
                                        VescIf.setWakeLock(true)
                                    }
                                } else {
                                    VescIf.closeRtLogFile()
                                    Utility.stopGnssForegroundService()

                                    if (!VescIf.useWakeLock()) {
                                        VescIf.setWakeLock(false)
                                    }
                                }
                            }

                            Timer {
                                repeat: true
                                running: true
                                interval: 500

                                onTriggered: {
                                    if (rtLogEnBox.checked
                                            && !VescIf.isRtLogOpen()) {
                                        Utility.stopGnssForegroundService()

                                        if (!VescIf.useWakeLock()) {
                                            VescIf.setWakeLock(false)
                                        }
                                    }

                                    rtLogEnBox.checked = VescIf.isRtLogOpen()
                                }
                            }
                        }
                }
                            GroupBox {
                               // id: batt6Box
                               // title: "Data Logging"
                                Layout.fillWidth: true
                                Layout.columnSpan: 1

                                RowLayout {
                                    anchors.fill: parent
                                    spacing: 5

                                    Button {
                                        text: "Choose Log\nDirectory..."
                                        Layout.preferredHeight: 70

                                        Layout.preferredWidth: 120
                                        onClicked: {
                                            if (Utility.requestFilePermission(
                                                        )) {
                                                logFilePicker.enabled = true
                                                logFilePicker.visible = true
                                            } else {
                                                VescIf.emitMessageDialog(
                                                            "File Permissions",
                                                            "Unable to request file system permission.",
                                                            false, false)
                                            }
                                        }
                                    }

                                    TextInput {
                                        color:{color=Utility.getAppHexColor("lightText")}
                                        id: rtLogFileText
                                        //anchors.fill: parent
                                        //anchors.margins: 7
                                        font.pointSize: 12
                                        text: "./log"

                                        QSettings.Settings {
                                            property alias rtLog: rtLogFileText.text
                                            property alias rtLogEnable: rtLogEnBox.checked
                                            property alias profileIndex: profilesBar.currentIndex

                                            property alias streetTorque: streetTorqueSlider.value
                                            property alias streetPas: streetPasSlider.value
                                            property alias streetPower: streetPowerSlider.value
                                            property alias streetRpm: streetRpmSlider.value
                                            property alias streetThrottleResponse: streetThrottleResponseSlider.value
                                            property alias streetThrottleExpo: streetThrottleExpoSlider.value
                                            property alias streetFixedThrottle: streetFixedThrottleCheckbox.checked

                                            property alias trailTorque: trailTorqueSlider.value
                                            property alias trailPas: trailPasSlider.value
                                            property alias trailPower: trailPowerSlider.value
                                            property alias trailRpm: trailRpmSlider.value
                                            property alias trailThrottleResponse: trailThrottleResponseSlider.value
                                            property alias trailThrottleExpo: trailThrottleExpoSlider.value
                                            property alias trailFixedThrottle: trailFixedThrottleCheckbox.checked

                                            property alias ludiTorque: torqueSlider.value
                                            property alias ludiPas: pasSlider.value
                                            property alias ludiPower: powerSlider.value
                                            property alias ludiRpm: rpmSlider.value
                                            property alias ludiThrottleResponse: throttleResponseSlider.value
                                            property alias ludiThrottleExpo: throttleExpoSlider.value
                                            property alias ludiFixedThrottle: fixedThrottleCheckbox.checked

                                            property alias battCurrentMaxSetting: battCurrBox.realValue
                                            property alias battCellseSetting: battCellsBox.realValue
                                            property alias overVoltageSetting: battOvervoltageBox.realValue
                                            property alias underVoltageStartSetting: battUndervoltageStartBox.realValue
                                            property alias underVoltageEndSetting: battUndervoltageEndBox.realValue
                                            property alias wheelDiameterSetting: wheelDiameterBox.realValue
                                        }
                                    }
                                }
                            }

                            Item {
                                // Spacer
                                Layout.fillWidth: true
                                Layout.fillHeight: true
                            }
                        }
                    }

                    RowLayout {
                        id: bikeCfgRow
                        clip: false
                        visible: true
                        spacing: 5

                        Button {
                            text: "Read\nSettings"
                            Layout.columnSpan: 2
                            Layout.preferredWidth: 200
                            Layout.preferredHeight: 80
                            Layout.fillWidth: true
                            onClicked: {
                                tuneWriteSettingsButton.enabled = true
                                bikeWriteSettingsButton.enabled = true

                                mCommands.getMcconf()
                                mCommands.getAppConf()

                                // check for a match in power and phase current to select the profile
                                var useImperial = VescIf.useImperialUnits()
                                var impFact = useImperial ? 39.3701 : 1000.0

                                var power = mMcConf.getParamDouble("l_watt_max")
                                var torque = mMcConf.getParamDouble("l_current_max")
                                var rpm = mMcConf.getParamDouble("l_max_erpm") / 4.0 //(4 pole pairs)
                                var pas = mAppConf.getParamDouble("app_pas_conf.current_scaling") * torque
                                var throttleResponse = mAppConf.getParamDouble("app_adc_conf.ramp_time_pos")
                                var throttleExpo = 100.0 - (mAppConf.getParamDouble("app_adc_conf.throttle_exp")) * -20.0
                                var battCurr = mMcConf.getParamDouble("l_in_current_max")
                                var battOvervoltage = mMcConf.getParamDouble("l_max_vin")
                                var battUndervoltageStart = mMcConf.getParamDouble("l_battery_cut_start")
                                var battUndervoltageEnd = mMcConf.getParamDouble("l_battery_cut_end")
                                var battCells = mMcConf.getParamInt("si_battery_cells")
                                var wheelDiameter = mMcConf.getParamDouble("si_wheel_diameter") * impFact
                                var fixedThrottle

                                if ((power == streetPowerSlider.value) && (torque == streetTorqueSlider.value)) {
                                    profilesBar.setCurrentIndex(0)

                                    streetTorqueSlider.value = torque
                                    streetPowerSlider.value = power
                                    streetRpmSlider.value = rpm
                                    streetPasSlider.value = pas
                                    streetThrottleResponseSlider.value = throttleResponse
                                    streetThrottleExpoSlider.value = throttleExpo
                                }
                                if ((power == trailPowerSlider.value) && (torque == trailTorqueSlider.value)) {
                                    profilesBar.setCurrentIndex(1)

                                    trailTorqueSlider.value = torque
                                    trailPowerSlider.value = power
                                    trailRpmSlider.value = rpm
                                    trailPasSlider.value = pas
                                    trailThrottleResponseSlider.value = throttleResponse
                                    trailThrottleExpoSlider.value = throttleExpo
                                }
                                if ((power == powerSlider.value) && (torque == torqueSlider.value)) {
                                    profilesBar.setCurrentIndex(2)
                                    torqueSlider.value = torque
                                    powerSlider.value = power
                                    rpmSlider.value = rpm
                                    pasSlider.value = pas
                                    throttleResponseSlider.value = throttleResponse
                                    throttleExpoSlider.value = throttleExpo
                                }
                                battCurrBox.realValue = mMcConf.getParamDouble("l_in_current_max")
                                battCellsBox.realValue = mMcConf.getParamInt("si_battery_cells")
                                battOvervoltageBox.realValue = mMcConf.getParamDouble("l_max_vin")
                                battUndervoltageStartBox.realValue = mMcConf.getParamDouble("l_battery_cut_start")
                                battUndervoltageEndBox.realValue = mMcConf.getParamDouble("l_battery_cut_end")
                                wheelDiameterBox.realValue = mMcConf.getParamDouble("si_wheel_diameter") * impFact
                            }
                        }
                        Button {
                            id: bikeWriteSettingsButton
                            text: "Write\nSettings"
                            enabled: false
                            Layout.columnSpan: 2
                            Layout.preferredWidth: 200
                            Layout.preferredHeight: 80
                            Layout.fillWidth: true
                            onClicked: {
                                var useImperial = VescIf.useImperialUnits()
                                var impFact = useImperial ? 39.3701 : 1000.0

                                if(profilesBar.currentIndex == 0) {
                                    mMcConf.updateParamDouble("l_current_max", streetTorqueSlider.value)
                                    mMcConf.updateParamDouble("l_watt_max", streetPowerSlider.value)
                                    mMcConf.updateParamDouble("l_max_erpm", streetRpmSlider.value * 4.0)
                                    mAppConf.updateParamDouble("app_pas_conf.current_scaling", streetPasSlider.value / streetTorqueSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.ramp_time_pos", streetThrottleResponseSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.throttle_exp", (100.0 - streetThrottleExpoSlider.value) / -20.0)
                                    fixedThrottle = streetFixedThrottleCheckbox.checked
                                }
                                if(profilesBar.currentIndex == 1) {
                                    mMcConf.updateParamDouble("l_current_max", trailTorqueSlider.value)
                                    mMcConf.updateParamDouble("l_watt_max", trailPowerSlider.value)
                                    mMcConf.updateParamDouble("l_max_erpm", trailRpmSlider.value * 4.0)
                                    mAppConf.updateParamDouble("app_pas_conf.current_scaling", trailPasSlider.value / trailTorqueSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.ramp_time_pos", trailThrottleResponseSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.throttle_exp", (100.0 - trailThrottleExpoSlider.value) / -20.0)
                                    fixedThrottle = trailFixedThrottleCheckbox.checked
                                }
                                if(profilesBar.currentIndex == 2) {
                                    mMcConf.updateParamDouble("l_current_max", torqueSlider.value)
                                    mMcConf.updateParamDouble("l_watt_max", powerSlider.value)
                                    mMcConf.updateParamDouble("l_max_erpm", rpmSlider.value * 4.0)
                                    mAppConf.updateParamDouble("app_pas_conf.current_scaling", pasSlider.value / torqueSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.ramp_time_pos", throttleResponseSlider.value)
                                    mAppConf.updateParamDouble("app_adc_conf.throttle_exp", (100.0 - throttleExpoSlider.value) / -20.0)
                                    fixedThrottle = fixedThrottleCheckbox.checked
                                }
                                mMcConf.updateParamDouble("l_in_current_max", battCurrBox.realValue)
                                mMcConf.updateParamInt("si_battery_cells", battCellsBox.realValue)
                                mMcConf.updateParamDouble("l_max_vin",battOvervoltageBox.realValue)
                                mMcConf.updateParamDouble("l_battery_cut_start",battUndervoltageStartBox.realValue)
                                mMcConf.updateParamDouble("l_battery_cut_end",battUndervoltageEndBox.realValue)
                                mMcConf.updateParamDouble("si_wheel_diameter",wheelDiameterBox.realValue / impFact)
                                scheduleConfWrite = true
                            }
                        }
                    }
                }
                DirectoryPicker {
                    id: logFilePicker
                    anchors.fill: parent
                    showDotAndDotDot: true
                    visible: false
                    enabled: false

                    onDirSelected: {
                        rtLogFileText.text = fileName
                    }
                }
            }

            Page {
                background: Rectangle {
                    opacity: 0.0
                }

                FwUpdate {
                    anchors.fill: parent
                }
            }
        }
    }

    Connections {
        target: mMcConf

        function onUpdated() {
            confTimer.mcConfRx = true
        }
    }

    Connections {
        target: mAppConf

        function onUpdated() {
            confTimer.appConfRx = true
        }
    }

    Timer {
        id: statusTimer
        interval: 1600
        running: false
        repeat: false
        onTriggered: {
            connectedText.text = VescIf.getConnectedPortName()
            connectedRect.color = "#4f4f4f"
        }
    }

    Timer {
        id: uiTimer
        interval: 1000
        running: true
        repeat: true
        onTriggered: {
           // if (!statusTimer.running && connectedText.text !== VescIf.getConnectedPortName()) {
             //   connectedText.text = VescIf.getConnectedPortName()
            //}
        }
    }

    Timer {
        id: confTimer
        interval: 1000
        running: true
        repeat: true

        property bool mcConfRx: false
        property bool appConfRx: false
        property int writingStage: 0

        onTriggered: {
            VescIf.setSpeedGaugeUseNegativeValues(false)
            if(tabBar.currentIndex !== 3) {// don't poll in the firmware update page
                if (VescIf.isPortConnected() && VescIf.getLastFwRxParams().hwTypeStr() === "VESC") {
                    if(scheduleConfWrite) {
                        if(writingStage === 0) {
                            mCommands.setMcconf(true)
                            writingStage = 1
                        } else {
                            if(writingStage === 1) {
                                mCommands.getMcconf()//update current_scale now
                                writingStage = 2
                            } else {
                                if(writingStage === 2) {
                                    mCommands.setAppConf(true)
                                    writingStage = 3
                                } else {
                                    if(fixedThrottle) {
                                        mCommands.sendTerminalCmd("fix_throttle 1")
                                    } else {
                                        mCommands.sendTerminalCmd("fix_throttle 0")
                                    }
                                    writingStage = 0
                                    scheduleConfWrite = false
                                }
                            }
                        }
                    }
                    if (!mcConfRx) {
                        mCommands.getMcconf()
                    }
                    if (!appConfRx) {
                        mCommands.getAppConf()
                    }
                }
            }
        }
    }
    Timer {
        id: rtTimer
        interval: 50
        running: true
        repeat: true
        onTriggered: {
            if (VescIf.isPortConnected()) {
                // Sample RT data when the corresponding page is selected, or when
                // RT logging is active.
                if (VescIf.isRtLogOpen()) {
                    interval = 50
                    mCommands.getValues()
                    mCommands.getValuesSetup()
                    //mCommands.getImuData(0xFFFF)
                    //mCommands.bmsGetValues()
                } else {
                    if (tabBar.currentIndex == 0) {
                        interval = 50
                        mCommands.getValuesSetup()
                    }
                }
            }
        }
    }
}
