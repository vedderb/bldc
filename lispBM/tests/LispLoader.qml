import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls.Material 2.2
import Vedder.vesc.utility 1.0
import Vedder.vesc.logreader 1.0
import Vedder.vesc.commands 1.0

Item {
    id: mainItem
    anchors.fill: parent
    anchors.margins: 5
    
    property var cnt: 0
    property Commands mCommands: VescIf.commands()
    
    LogReader {
        id: mLogReader
    }
    
    ColumnLayout {
        anchors.fill: parent
           
        Text {
            Layout.fillWidth: true
            Layout.fillHeight: true
            text: "Go to Settings > Paths to change the source and destination directories."
            color: "white"
            wrapMode: Text.WordWrap
        }
        
        Text {
            Layout.fillWidth: true
            Layout.fillHeight: true
            id: txt
            text: ""
            color: "white"
        }
        
        RowLayout {
            Layout.fillWidth: true
            
            Button {
                Layout.fillWidth: true
                text: "Read and upload"
                
                onClicked: {
//                    mLogReader.openLogFile("example_duty.lisp")
//                    mLogReader.openLogFile("example_ppm_read.lisp")
//                    mLogReader.openLogFile("example_control_servo_from_encoder.lisp")
//                    mLogReader.openLogFile("example_control_servo_from_duty.lisp")
//                    mLogReader.openLogFile("example_print_bms_data.lisp")
                    mLogReader.openLogFile("example_can_pos_follow.lisp")

//                    mLogReader.openLogFile("test_math.lisp")
                    
                    mCommands.qmlUiErase()
                    if (Utility.waitSignal(mCommands, "2eraseQmluiResReceived(bool)", 4000)) {
                        console.log("Erase OK")
                    }
                    
                    var offset = 0
                    
                    var text = ""
                    for (var i = 0;i < 500;i++) {
                        var line = mLogReader.readLine()
                        text += line
                        
                        mCommands.qmlUiWrite(line, offset)
                        
                        offset += line.length
                        
                        if (mLogReader.atEnd()) {
                            console.log("File ended")
                            break
                        }
                    }
                    txt.text = text
                    
                    mCommands.qmlUiWrite("\0", offset)
                    
                    mLogReader.closeLogFile()
                }
            }
            
            Button {
                Layout.fillWidth: true
                text: "Run"
                
                onClicked: {
                    mCommands.sendTerminalCmd("lisp_run")
                }
            }
            
            Button {
                Layout.fillWidth: true
                text: "Stop"
                
                onClicked: {
                    mCommands.sendTerminalCmd("lisp_stop")
                }
            }
        }
    }
}
