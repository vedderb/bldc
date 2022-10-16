/*
    Copyright 2022 Benjamin Vedder benjamin@vedder.se

    This file is part of the VESC firmware.

    The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import QtQuick 2.5
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

import Vedder.vesc.commands 1.0
import Vedder.vesc.configparams 1.0

Item {
    id: container
    anchors.fill: parent
    anchors.margins: 10
    
    property Commands mCommands: VescIf.commands()
    
    ColumnLayout {
        anchors.fill: parent
        
        DoubleSpinBox {
            id: sb
            Layout.fillWidth: true
        }
        
        Item {
            Layout.fillHeight: true
        }
    }
    
    // Send the value of the spinbox every second
    Timer {
        running: true
        repeat: true
        interval: 1000
        
        onTriggered: {
            var buffer = new ArrayBuffer(4)
            var dv = new DataView(buffer)
            var ind = 0
            dv.setFloat32(ind, sb.realValue); ind += 4
            mCommands.sendCustomAppData(buffer)
        }
    }
    
    // Print the message counter and value that we sent the last time
    Connections {
        target: mCommands
        
        onCustomAppDataReceived: {
            var dv = new DataView(data)
            var ind = 0
            var msg_cnt = dv.getInt32(ind); ind += 4
            var msg_val = dv.getFloat32(ind); ind += 4
            console.log(msg_cnt + " " + msg_val)
        }
    }
}
