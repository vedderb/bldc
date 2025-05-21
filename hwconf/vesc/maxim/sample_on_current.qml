import QtQuick 2.15
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls.Material 2.2

import Vedder.vesc.commands 1.0
import Vedder.vesc.utility 1.0
import Vedder.vesc.qminimp3 1.0

Item {
    id: mainItem
    anchors.fill: parent
    anchors.margins: 5

    property Commands mCommands: VescIf.commands()
    property var sampleDone: false

    Component.onCompleted: {

    }

    Timer {
        running: true
        repeat: true
        interval: 10

        onTriggered: {
            if (!sampleDone) {
                mCommands.getValues()
            }
        }
    }

    ColumnLayout {
        anchors.fill: parent

        Text {
            Layout.fillWidth: true
            Layout.fillHeight: true
            text: "Test"
            color: "white"
            wrapMode: Text.WordWrap
        }

        Button {
            Layout.fillWidth: true
            text: "Restart"

            onClicked: {
                sampleDone = false
            }
        }
    }

    Connections {
        target: mCommands

        function onValuesReceived(values, mask) {
            if (values.current_motor > 800 && values.duty_now > 0.5 && !sampleDone) {
                sampleDone = true
                mCommands.samplePrintQml(1, 800, 1, false)
                console.log("Sampling Started!\n" +
                            "Current: " + values.current_motor + "\n" +
                            "Duty   : " + values.duty_now + "\n" +
                            "Power  : " + values.current_in * values.v_in
                            )
            }

            if (values.fault_str != "FAULT_CODE_NONE") {
                console.log("Fault: " + values.fault_str)
                sampleDone = true
            }
        }
    }
}
