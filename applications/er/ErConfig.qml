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
    
    property bool readSettingsDone: false
    property var er_msg: []
    property var er_set: []
    property var er_io: []
    
    Component.onCompleted: {        
        er_msg.ER_MSG_SET_MODE_PARAMS = 0
        er_msg.ER_MSG_GET_MODE_PARAMS = 1
        er_msg.ER_MSG_GET_IO = 2
        er_msg.ER_MSG_RESTORE_SETTINGS = 3
        er_msg.ER_MSG_SET_MOTORS_ENABLED = 4
        er_msg.ER_MSG_SET_PEDAL_TEST_MODE = 5
        
        er_set.p_throttle_hyst = 0.04
        er_set.p_pedal_current = 20.0
        er_set.p_start_gain = 4.0
        er_set.p_start_gain_end_speed = 15.0
        er_set.p_output_power = 1.0
        er_set.p_top_speed_erpm = 2000
        er_set.p_brake_current_front = 0.5
        er_set.p_brake_current_rear = 0.5
        er_set.p_brake_current_both = 1.0
        
        er_io.mode_btn = false
        er_io.brake_front = false
        er_io.brake_rear = false
        er_io.kill_sw = false
        er_io.led_eco_on = false
        er_io.led_sport_on = false
        er_io.led_low_batt_on = false
        er_io.led_fault_on = false
        er_io.stand = false
    }
    
    Timer {
        repeat: true
        interval: 200
        running: true
        
        onTriggered: {
            readIo()
        }
    }
    
    Connections {
        target: mCommands
        
        onCustomAppDataReceived: {
            var dv = new DataView(data, 0)
            var ind = 0
            var cmd = dv.getUint8(ind++)
            
            if (cmd == er_msg.ER_MSG_SET_MODE_PARAMS) {
                VescIf.emitStatusMessage("ER Params Set OK", true)
            } else if (cmd == er_msg.ER_MSG_GET_MODE_PARAMS) {
                er_set.p_throttle_hyst = dv.getFloat32(ind); ind += 4
                er_set.p_pedal_current = dv.getFloat32(ind); ind += 4
                er_set.p_start_gain = dv.getFloat32(ind); ind += 4
                er_set.p_start_gain_end_speed = dv.getFloat32(ind); ind += 4
                er_set.p_output_power = dv.getFloat32(ind); ind += 4
                er_set.p_top_speed_erpm = dv.getFloat32(ind); ind += 4
                er_set.p_brake_current_front = dv.getFloat32(ind); ind += 4
                er_set.p_brake_current_rear = dv.getFloat32(ind); ind += 4
                er_set.p_brake_current_both = dv.getFloat32(ind); ind += 4
                readSettingsDone = true
                updateSliders()
            } else if (cmd == er_msg.ER_MSG_GET_IO) {
                er_io.mode_btn = dv.getUint8(ind); ind += 1
                er_io.brake_front = dv.getUint8(ind); ind += 1
                er_io.brake_rear = dv.getUint8(ind); ind += 1
                er_io.kill_sw = dv.getUint8(ind); ind += 1
                
                er_io.led_eco_on = dv.getUint8(ind); ind += 1
                er_io.led_sport_on = dv.getUint8(ind); ind += 1
                er_io.led_low_batt_on = dv.getUint8(ind); ind += 1
                er_io.led_fault_on = dv.getUint8(ind); ind += 1
                
                er_io.stand = dv.getUint8(ind); ind += 1
                
                updateIoStatus()
            } else if (cmd == er_msg.ER_MSG_SET_MOTORS_ENABLED) {
                if (dv.getUint8(ind++)) {
                    VescIf.emitStatusMessage("ER Motors Enabled", true)
                } else {
                    VescIf.emitStatusMessage("ER Motors Disabled", true)
                }
            } else if (cmd == er_msg.ER_MSG_SET_PEDAL_TEST_MODE) {
                if (dv.getUint8(ind++)) {
                    VescIf.emitStatusMessage("Pedal Test Enabled", true)
                } else {
                    VescIf.emitStatusMessage("Pedal Test Disabled", true)
                }
            }
        }
    }
    
    function readSettings() {
        var buffer = new ArrayBuffer(1)
        var dv = new DataView(buffer)
        var ind = 0
        dv.setUint8(ind++, er_msg.ER_MSG_GET_MODE_PARAMS)
        mCommands.sendCustomAppData(buffer)
    }
    
    function writeSettings() {
        if (!readSettingsDone) {
            return
        }
        
        var buffer = new ArrayBuffer(37)
        var dv = new DataView(buffer)
        var ind = 0
        dv.setUint8(ind, er_msg.ER_MSG_SET_MODE_PARAMS); ind += 1
        dv.setFloat32(ind, er_set.p_throttle_hyst); ind += 4
        dv.setFloat32(ind, er_set.p_pedal_current); ind += 4
        dv.setFloat32(ind, er_set.p_start_gain); ind += 4
        dv.setFloat32(ind, er_set.p_start_gain_end_speed); ind += 4
        dv.setFloat32(ind, er_set.p_output_power); ind += 4
        dv.setFloat32(ind, er_set.p_top_speed_erpm); ind += 4
        dv.setFloat32(ind, er_set.p_brake_current_front); ind += 4
        dv.setFloat32(ind, er_set.p_brake_current_rear); ind += 4
        dv.setFloat32(ind, er_set.p_brake_current_both); ind += 4
        mCommands.sendCustomAppData(buffer)
    }
    
    function restoreSettings() {
        var buffer = new ArrayBuffer(1)
        var dv = new DataView(buffer)
        var ind = 0
        dv.setUint8(ind++, er_msg.ER_MSG_RESTORE_SETTINGS)
        mCommands.sendCustomAppData(buffer)
    }
    
    function readIo() {
        var buffer = new ArrayBuffer(1)
        var dv = new DataView(buffer)
        var ind = 0
        dv.setUint8(ind++, er_msg.ER_MSG_GET_IO)
        mCommands.sendCustomAppData(buffer)
    }
    
    function setMotorsEnabled(enabled) {
        var buffer = new ArrayBuffer(2)
        var dv = new DataView(buffer)
        var ind = 0
        dv.setUint8(ind++, er_msg.ER_MSG_SET_MOTORS_ENABLED)
        dv.setUint8(ind++, enabled)
        mCommands.sendCustomAppData(buffer)
    }
    
    function setPedalTestMode(enabled) {
        var buffer = new ArrayBuffer(2)
        var dv = new DataView(buffer)
        var ind = 0
        dv.setUint8(ind++, er_msg.ER_MSG_SET_PEDAL_TEST_MODE)
        dv.setUint8(ind++, enabled)
        mCommands.sendCustomAppData(buffer)
    }
    
    function updateSliders() {
        repSliders.itemAt(0).value = er_set.p_pedal_current
        repSliders.itemAt(1).value = er_set.p_start_gain
        repSliders.itemAt(2).value = er_set.p_start_gain_end_speed
        repSliders.itemAt(3).value = er_set.p_output_power
        repSliders.itemAt(4).value = er_set.p_top_speed_erpm
        repSliders.itemAt(5).value = er_set.p_brake_current_front
        repSliders.itemAt(6).value = er_set.p_brake_current_rear
        repSliders.itemAt(7).value = er_set.p_brake_current_both
    }
    
    function updateIoStatus() {
        ioRepeater.itemAt(0).color = (er_io.mode_btn ? "green": "darkGray")
        ioRepeater.itemAt(1).color = (er_io.brake_front ? "green" : "darkGray")
        ioRepeater.itemAt(2).color = (er_io.brake_rear ? "green" : "darkGray")
        ioRepeater.itemAt(3).color = (er_io.kill_sw ? "green" : "darkGray")
        ioRepeater.itemAt(4).color = (er_io.led_eco_on ? "green" : "darkGray")
        ioRepeater.itemAt(5).color = (er_io.led_sport_on ? "green" : "darkGray")
        ioRepeater.itemAt(6).color = (er_io.led_low_batt_on ? "green" : "darkGray")
        ioRepeater.itemAt(7).color = (er_io.led_fault_on ? "green" : "darkGray")
        ioRepeater.itemAt(8).color = (er_io.stand ? "green" : "darkGray")
        
        var title_new = ""
        if (er_io.led_eco_on) {
            title_new = "Eco Settings"
        } else if (er_io.led_sport_on) {
            title_new = "Sport Settings"
        } else {
            title_new = "Normal Settings"
        }
        
        if (settingsBox.title != title_new) {
            readSettings()
        }
        
        settingsBox.title = title_new
    }
    
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
    
    ScrollView {
        anchors.fill: parent
        clip: true
        contentWidth: availableWidth
        
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
                        
                        buttonText: "Setup\nRear\nMotor"
                        imageSrc: "qrc:/res/icons/motor.png"
                        
                        onClicked: {
                            setupRearDialog.open()
                        }
                    }
                    
                    ImageButton {
                        Layout.fillWidth: true
                        Layout.preferredWidth: 500
                        Layout.preferredHeight: 80
                        
                        buttonText: "Setup\nFront\nMotor"
                        imageSrc: "qrc:/res/icons/motor.png"
                        
                        onClicked: {
                            setupFrontDialog.open()
                        }
                    }
                    
                    ImageButton {
                        Layout.fillWidth: true
                        Layout.preferredWidth: 500
                        Layout.preferredHeight: 80
                        
                        buttonText: "Restore\nPedal\nSettings"
                        imageSrc: "qrc:/res/icons/Restart-96.png"
                        
                        onClicked: {
                            restoreSettings()
                            readSettings()
                        }
                    }
                    
                    CheckBox {
                        Layout.fillWidth: true
                        Layout.preferredWidth: 500
                        Layout.preferredHeight: 80
                        text: "Pedal Test"
                        
                        onToggled: {
                            setPedalTestMode(checked)
                        }
                    }
                }
            }
            
            GroupBox {
                title: qsTr("IO Status")
                Layout.fillWidth: true
                
                GridLayout {
                    anchors.fill: parent
                    columns: 2
                    
                    Repeater {
                        id: ioRepeater
                        
                        model: ["mode_btn", "brake_front", "brake_rear", "kill_sw",
                            "led_eco_on", "led_sport_on", "led_low_batt_on", "led_fault_on",
                            "stand"]
                            
                        Rectangle {
                            Layout.fillWidth: true
                            radius: 5
                            height: 30
                            color: "darkGray"
                            
                            Text {
                                anchors.centerIn: parent
                                text: modelData
                                color: "white"
                            }
                        }
                    }
                }
            }
            
            GroupBox {
                id: settingsBox
                title: qsTr("Settings for selected mode")
                Layout.fillWidth: true
                
                GridLayout {
                    id: sliderGrid
                    
                    anchors.fill: parent
                    columns: 3
                    rows: repSliders.count
                    flow: GridLayout.TopToBottom
                    
                    // Name, min, max, val, unitFactor, decimals, unit, propertyName
                    property var repModel: [
                    ["Pedal\nCurrent", 1.5, 50, 15, 1, 1, " A", "p_pedal_current"],
                    ["Start Gain", 1, 10, 4, 1, 1, "", "p_start_gain"],
                    ["Start Gain\nEnd Speed", 1, 100, 15, 1, 1, "\nkm/h", "p_start_gain_end_speed"],
                    ["Power", 0, 1, 0.8, 100, 0, " %", "p_output_power"],
                    ["Max Power\nERPM", 210, 5000, 2000, 1, 0, "", "p_top_speed_erpm"],
                    ["Front\nBrake", 0, 1, 0.5, 100, 0, " %", "p_brake_current_front"],
                    ["Rear\nBrake", 0, 1, 0.5, 100, 0, " %", "p_brake_current_rear"],
                    ["Both\nBrakes", 0, 1, 1, 100, 0, " %", "p_brake_current_both"],
                    ]
                    
                    Repeater {
                        model: sliderGrid.repModel
                        
                        Text {
                            color: "white"
                            text: modelData[0]
                        }
                    }
                    
                    Repeater {
                        id: repSliders
                        
                        model: sliderGrid.repModel
                        
                        Slider {
                            Layout.fillWidth: true
                            from: modelData[1]; to: modelData[2]; value: modelData[3]
                            
                            onValueChanged: {
                                repUnit.itemAt(index).text = parseFloat(value * modelData[4]).toFixed(modelData[5]) + modelData[6]
                            }
                            
                            onPressedChanged: {
                                if (!pressed) {
                                    er_set[modelData[7]] = value
                                    writeSettings()
                                }
                            }
                        }
                    }
                    
                    Repeater {
                        id: repUnit
                        
                        model: sliderGrid.repModel
                        
                        Text {
                            color: "white"
                            text: parseFloat(repSliders.itemAt(index).value * modelData[4]).toFixed(modelData[5]) + modelData[6]
                        }
                    }
                }
            }
            
            Item {
                Layout.fillHeight: true
            }
        }
    }
    
    function disableDialog() {
        commDialog.open()
        mainColumn.enabled = false
        setMotorsEnabled(false)
    }

    function enableDialog() {
        commDialog.close()
        mainColumn.enabled = true
        setMotorsEnabled(true)
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
        parent: container

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
            
            mMcConf.updateParamDouble("si_gear_ratio", 112 / 18, null)
            mMcConf.updateParamInt("si_motor_poles", 8, null)
            mMcConf.updateParamDouble("si_wheel_diameter", 0.58, null)
            
            mMcConf.updateParamEnum("si_battery_type", 0, null)
            mMcConf.updateParamInt("si_battery_cells", 14, null)
            mMcConf.updateParamDouble("si_battery_ah", 120, null)
            
            mMcConf.updateParamDouble("l_current_max", 380, null)
            mMcConf.updateParamDouble("l_current_min", -120, null)
            mMcConf.updateParamDouble("l_in_current_max", 300, null)
            mMcConf.updateParamDouble("l_in_current_min", -100, null)
            mMcConf.updateParamDouble("l_abs_current_max", 480, null)
            
            mMcConf.updateParamDouble("l_min_vin", 20, null)
            mMcConf.updateParamDouble("l_max_vin", 70, null)
            mMcConf.updateParamDouble("l_battery_cut_start", 47.6, null)
            mMcConf.updateParamDouble("l_battery_cut_end", 42, null)
            
            mMcConf.updateParamDouble("l_temp_motor_start", 125, null)
            mMcConf.updateParamDouble("l_temp_motor_end", 135, null)
            mMcConf.updateParamDouble("l_temp_accel_dec", 0, null)
            
            mMcConf.updateParamDouble("foc_f_zv", 30000, null)
            mMcConf.updateParamEnum("foc_sensor_mode", 2, null)
            mMcConf.updateParamDouble("foc_openloop_rpm", 350, null)
            mMcConf.updateParamDouble("foc_sl_erpm", 6000, null)
            mMcConf.updateParamBool("foc_sample_high_current", 1, null)
            mMcConf.updateParamDouble("foc_phase_filter_max_erpm", 1600, null)
            
            // Field Weakening
            mMcConf.updateParamDouble("foc_fw_current_max", 40, null)
            
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
            mMcConf.updateParamDouble("foc_motor_l", rl[1] * 0.9, null)
                        
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
            
            mMcConf.updateParamInt("foc_hall_table__0", hall[1], null)
            mMcConf.updateParamInt("foc_hall_table__1", hall[2], null)
            mMcConf.updateParamInt("foc_hall_table__2", hall[3], null)
            mMcConf.updateParamInt("foc_hall_table__3", hall[4], null)
            mMcConf.updateParamInt("foc_hall_table__4", hall[5], null)
            mMcConf.updateParamInt("foc_hall_table__5", hall[6], null)
            mMcConf.updateParamInt("foc_hall_table__6", hall[7], null)
            mMcConf.updateParamInt("foc_hall_table__7", hall[8], null)
            
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
            
            mMcConf.updateParamBool("m_invert_direction", 1, null)
            
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
