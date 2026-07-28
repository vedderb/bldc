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

import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

ApplicationWindow {
  id: root
  visible: true
  // Sensible desktop default; Android overrides with the actual screen size.
  width:  480
  height: 800

  // ── LispBM UI area ──────────────────────────────────────────────────────
  // The scenario script builds its entire widget tree inside this item via
  // the qt-* extensions.  It fills the window minus whatever the log pane
  // occupies at the bottom.
  Item {
    id: lbmArea
    objectName: "lbmArea"
    anchors {
      top:    parent.top
      left:   parent.left
      right:  parent.right
      bottom: logPane.top
    }
  }

  // ── Log pane (hidden by default) ────────────────────────────────────────
  // Shows LispBM print output and eval results.  Slide up from the bottom.
  // On the emulator you can also follow output with: adb logcat -s Qt,lbm
  Rectangle {
    id: logPane
    anchors.bottom: parent.bottom
    width:  parent.width
    height: logPane.open ? parent.height * 0.32 : 0
    color:  "#dd111122"
    clip:   true

    property bool open: false

    Behavior on height { NumberAnimation { duration: 180; easing.type: Easing.OutCubic } }

    ScrollView {
      anchors.fill:    parent
      anchors.margins: 4
      TextArea {
        id:           logText
        readOnly:     true
        color:        "#a8ff78"
        font.family:  "Monospace"
        font.pixelSize: 11
        background:   null
        wrapMode:     TextArea.Wrap
        selectByMouse: true
      }
    }
  }

  // ── Log toggle button ────────────────────────────────────────────────────
  // Sits in the bottom-right corner, just above the log pane.
  RoundButton {
    id: logToggle
    anchors.right:   parent.right
    anchors.bottom:  logPane.top
    anchors.margins: 8
    width:  36
    height: 36
    text:   logPane.open ? "▼" : "▲"
    font.pixelSize: 13
    opacity: 0.72
    onClicked: logPane.open = !logPane.open
  }

  // ── Wire up bridge output to the log pane ──────────────────────────────
  Connections {
    target: bridge
    function onOutputAppended(text) {
      logText.insert(logText.length, text)
      logText.cursorPosition = logText.length
    }
    function onOutputCleared() {
      logText.clear()
    }
  }
}
