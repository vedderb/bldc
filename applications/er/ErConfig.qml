import QtQuick 2.5
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

import Vedder.vesc.commands 1.0
import Vedder.vesc.configparams 1.0
import Vedder.vesc.utility 1.0

Item {
    id: container
    anchors.fill: parent
    anchors.margins: 10
    
    property Commands mCommands: VescIf.commands()
    property ConfigParams mMcConf: VescIf.mcConfig()
    property ConfigParams mAppConf: VescIf.appConfig()
    property var canDevs: []
    
    function getCanIds() {
        if (canDevs.length == 0) {
            disableDialog()
            canDevs = Utility.scanCanVescOnly(VescIf)
            enableDialog()
        }
    }
    
    function selectRearMotor() {
        var res = true
        
        if (mCommands.getSendCan()) {
            VescIf.canTmpOverride(false, 0)
        } else {
            getCanIds()
            if (canDevs.length != 0) {
                VescIf.canTmpOverride(true, canDevs[0])
            } else {
                res = false
            }
        }
        
        if (!res) {
            VescIf.emitMessageDialog("Select Rear Motor",
                "Could not select rear motor. Make sure that the CAN-bus is plugged in.",
                false, false)
        }
        
        return res
    }
    
    function unselectRearMotor() {
        VescIf.canTmpOverrideEnd()
        
        mCommands.getMcconf()
        if (!Utility.waitSignal(mMcConf, "2updated()", 4000)) {
            VescIf.emitMessageDialog("Read Configuration",
            "Could not read motor configuration.",
            false, false)
        }
    }
    
    ColumnLayout {
        id: mainColumn
        
        anchors.fill: parent
        
        Text {
            Layout.fillWidth: true
            color: "White"
            horizontalAlignment: Text.AlignHCenter
            font.pointSize: 20
            text: "Erockit Console"
        }
        
        GroupBox {
            id: wizardBox
            title: qsTr("Quick Setup")
            Layout.fillWidth: true
            
            GridLayout {
                anchors.topMargin: -5
                anchors.bottomMargin: -5
                anchors.fill: parent
                columns: 2
                columnSpacing: 5
                rowSpacing: 0
                
                ImageButton {
                    Layout.fillWidth: true
                    Layout.preferredWidth: 500
                    Layout.preferredHeight: 80
                    
                    buttonText: "Setup\nRear Motor"
                    imageSrc: "qrc:/res/icons/motor.png"
                    
                    onClicked: {
                        setupRearDialog.open()
                    }
                }
                
                ImageButton {
                    Layout.fillWidth: true
                    Layout.preferredWidth: 500
                    Layout.preferredHeight: 80
                    
                    buttonText: "Setup\nFront Motor"
                    imageSrc: "qrc:/res/icons/motor.png"
                    
                    onClicked: {
                        setupFrontDialog.open()
                    }
                }
            }
        }
        
        Item {
            Layout.fillHeight: true
        }
    }
    
    function disableDialog() {
        commDialog.open()
        mainColumn.enabled = false
    }

    function enableDialog() {
        commDialog.close()
        mainColumn.enabled = true
    }

    Dialog {
        id: commDialog
        title: "Processing..."
        closePolicy: Popup.NoAutoClose
        modal: true
        focus: true

        width: parent.width - 20
        x: 10
        y: parent.height / 2 - height / 2
        parent: ApplicationWindow.overlay

        ProgressBar {
            anchors.fill: parent
            indeterminate: visible
        }
    }
    
    Dialog {
        id: setupRearDialog
        standardButtons: Dialog.Ok | Dialog.Cancel
        modal: true
        focus: true
        rightMargin: 10
        leftMargin: 10
        closePolicy: Popup.CloseOnEscape
        title: "Setup Rear Motor Parameters"
        parent: container
        property var canDevs: []

        y: parent.y + parent.height / 2 - height / 2

        ColumnLayout {
            anchors.fill: parent

            Text {
                Layout.fillWidth: true
                color: "#ffffff"
                verticalAlignment: Text.AlignVCenter
                wrapMode: Text.WordWrap
                text: "This is going to spin up the rear motor. Make " +
                      "sure that nothing is in the way."
            }
        }

        onAccepted: {
            if (!selectRearMotor()) {
                return
            }
            
            disableDialog()
            
            mCommands.getMcconf()
            if (!Utility.waitSignal(mMcConf, "2updated()", 4000)) {
                VescIf.emitMessageDialog("Read Configuration",
                    "Could not read motor configuration.",
                    false, false)
                unselectRearMotor()
                enableDialog()
                return
            }
            
            mMcConf.updateParamDouble("si_gear_ratio", 5.6, null)
            mMcConf.updateParamInt("si_motor_poles", 8, null)
            mMcConf.updateParamDouble("si_wheel_diameter", 0.58, null)
            
            mMcConf.updateParamDouble("l_current_max", 380, null)
            mMcConf.updateParamDouble("l_current_min", -120, null)
            mMcConf.updateParamDouble("l_in_current_max", 300, null)
            mMcConf.updateParamDouble("l_in_current_min", -100, null)
            mMcConf.updateParamDouble("l_abs_current_max", 480, null)
            
            mMcConf.updateParamDouble("l_min_vin", 20, null)
            mMcConf.updateParamDouble("l_max_vin", 70, null)
            mMcConf.updateParamDouble("l_battery_cut_start", 47.6, null)
            mMcConf.updateParamDouble("l_battery_cut_end", 42, null)
            
            mMcConf.updateParamDouble("l_temp_motor_start", 90, null)
            mMcConf.updateParamDouble("l_temp_motor_end", 105, null)
            mMcConf.updateParamDouble("l_temp_accel_dec", 0, null)
            
            mMcConf.updateParamDouble("foc_f_sw", 30000, null)
            mMcConf.updateParamEnum("foc_sensor_mode", 2, null)
            mMcConf.updateParamDouble("foc_openloop_rpm", 350, null)
            mMcConf.updateParamDouble("foc_sl_erpm", 6000, null)
            mMcConf.updateParamBool("foc_sample_high_current", 1, null)
            mMcConf.updateParamDouble("foc_phase_filter_max_erpm", 1600, null)
            
            mMcConf.updateParamEnum("m_motor_temp_sens_type", 4, null)
            mMcConf.updateParamInt("m_hall_extra_samples", 4, null)
                                          
            mCommands.setMcconf(false)
            if (!Utility.waitSignal(mCommands, "2ackReceived(QString)", 4000)) {
                VescIf.emitMessageDialog("Write Configuration",
                    "Could not write motor configuration.",
                    false, false)
                unselectRearMotor()
                enableDialog()
                return
            }
            
            // Resistance and inductance
            
            var rl = Utility.measureRLBlocking(VescIf)
            if (rl.length == 0 || rl[0] < 1e-10) {
                VescIf.emitMessageDialog("Measure RL",
                    "Could not measure resistance and inductance",
                    false, false)
                unselectRearMotor()
                enableDialog()
                return
            }
            
            rl[1] = rl[1] * 1e-6
            
            mMcConf.updateParamDouble("foc_motor_r", rl[0], null)
            mMcConf.updateParamDouble("foc_motor_l", rl[1], null)
                        
            // Flux linkage
            
            var linkage = Utility.measureLinkageOpenloopBlocking(VescIf, 100, 2000, 0.3, rl[0], rl[1])
            if (linkage <= 1e-10) {
                VescIf.emitMessageDialog("Measure Flux Linkage",
                    "Could not measure flux linkage",
                    false, false)
                unselectRearMotor()
                enableDialog()
                return
            }
            
            Utility.waitMotorStop(VescIf, 50, 6000)
            
            mMcConf.updateParamDouble("foc_motor_flux_linkage", linkage, null)
            
            // Calculate current controller and observer gains
            mMcConf.updateParamDouble("foc_observer_gain", (1.0e-3 / (linkage * linkage)) * 1e6, null)
            var tc = 200e-6
            var bw = 1.0 / tc
            mMcConf.updateParamDouble("foc_current_kp", rl[1] * bw, null)
            mMcConf.updateParamDouble("foc_current_ki", rl[0] * bw, null)
            
            // Temperature
            
            var t_motor = Utility.getMcValuesBlocking(VescIf).temp_motor
            mMcConf.updateParamBool("foc_temp_comp", 1, null)
            mMcConf.updateParamDouble("foc_temp_comp_base_temp", t_motor, null)
            
            // Hall table
            
            var hall = Utility.measureHallFocBlocking(VescIf, 150)
            if (hall[0] != 0) {
                VescIf.emitMessageDialog("Measure Hall Sensors",
                    "Could not measure hall sensors",
                    false, false)
                unselectRearMotor()
                enableDialog()
                return
            }
            
            mMcConf.updateParamInt("hall_table__0", hall[1], null)
            mMcConf.updateParamInt("hall_table__1", hall[2], null)
            mMcConf.updateParamInt("hall_table__2", hall[3], null)
            mMcConf.updateParamInt("hall_table__3", hall[4], null)
            mMcConf.updateParamInt("hall_table__4", hall[5], null)
            mMcConf.updateParamInt("hall_table__5", hall[6], null)
            mMcConf.updateParamInt("hall_table__6", hall[7], null)
            mMcConf.updateParamInt("hall_table__7", hall[8], null)
            
            mCommands.setMcconf(false)
            if (!Utility.waitSignal(mCommands, "2ackReceived(QString)", 4000)) {
                VescIf.emitMessageDialog("Write Configuration",
                    "Could not write motor configuration.",
                    false, false)
                unselectRearMotor()
                enableDialog()
                return
            }
            
            unselectRearMotor()
            enableDialog()

            VescIf.emitMessageDialog("Setup Rear Motor",
                "Done!",
                true, false)
        }
    }
    
    Dialog {
        id: setupFrontDialog
        standardButtons: Dialog.Ok | Dialog.Cancel
        modal: true
        focus: true
        rightMargin: 10
        leftMargin: 10
        closePolicy: Popup.CloseOnEscape
        title: "Setup Front Motor Parameters"
        parent: container
        property var canDevs: []

        y: parent.y + parent.height / 2 - height / 2

        ColumnLayout {
            anchors.fill: parent

            Text {
                Layout.fillWidth: true
                color: "#ffffff"
                verticalAlignment: Text.AlignVCenter
                wrapMode: Text.WordWrap
                text: "This is going to spin up the front motor. Make " +
                      "sure that nothing is in the way."
            }
        }

        onAccepted: {
            disableDialog()
            
            mCommands.getMcconf()
            if (!Utility.waitSignal(mMcConf, "2updated()", 4000)) {
                VescIf.emitMessageDialog("Read Configuration",
                    "Could not read motor configuration.",
                    false, false)
                enableDialog()
                return
            }
            
            // Resistance and inductance
            
            var rl = Utility.measureRLBlocking(VescIf)
            if (rl.length == 0 || rl[0] < 1e-10) {
                VescIf.emitMessageDialog("Measure RL",
                    "Could not measure resistance and inductance",
                    false, false)
                enableDialog()
                return
            }
            
            rl[1] = rl[1] * 1e-6
            
            mMcConf.updateParamDouble("foc_motor_r", rl[0], null)
            mMcConf.updateParamDouble("foc_motor_l", rl[1], null)
                        
            // Flux linkage
            
            var linkage = Utility.measureLinkageOpenloopBlocking(VescIf, 20, 2000, 0.2, rl[0], rl[1])
            if (linkage <= 1e-10) {
                VescIf.emitMessageDialog("Measure Flux Linkage",
                    "Could not measure flux linkage",
                    false, false)
                enableDialog()
                return
            }
            
            mMcConf.updateParamDouble("foc_motor_flux_linkage", linkage, null)
            
            Utility.waitMotorStop(VescIf, 50, 5000)
            
            // Calculate current controller and observer gains
            mMcConf.updateParamDouble("foc_observer_gain", (1.0e-3 / (linkage * linkage)) * 1e6, null)
            var tc = 600e-6
            var bw = 1.0 / tc
            mMcConf.updateParamDouble("foc_current_kp", rl[1] * bw, null)
            mMcConf.updateParamDouble("foc_current_ki", rl[0] * bw, null)
            
            // Temperature
            
            var t_motor = Utility.getMcValuesBlocking(VescIf).temp_motor
            mMcConf.updateParamBool("foc_temp_comp", 1, null)
            mMcConf.updateParamDouble("foc_temp_comp_base_temp", t_motor, null)
            
            mCommands.setMcconf(false)
            if (!Utility.waitSignal(mCommands, "2ackReceived(QString)", 4000)) {
                VescIf.emitMessageDialog("Write Configuration",
                    "Could not write motor configuration.",
                    false, false)
                enableDialog()
                return
            }
            
            enableDialog()

            VescIf.emitMessageDialog("Setup Front Motor",
                "Done!",
                true, false)
        }
    }
}
