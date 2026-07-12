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
  id:      root
  title:   "LispBM Editor (Qt Quick)"
  width:   1024
  height:  700
  visible: true

  // Ctrl+Return evaluates the current editor content.
  Shortcut {
    sequence: "Ctrl+Return"
    onActivated: bridge.evaluate(editor.text)
  }

  ColumnLayout {
    anchors.fill: parent
    anchors.margins: 6
    spacing: 4

    // ── Button bar ────────────────────────────────────────────────────────
    RowLayout {
      spacing: 6
      Layout.fillWidth: true

      Button {
        text: "Evaluate  (Ctrl+Return)"
        onClicked: bridge.evaluate(editor.text)
      }
      Button {
        text: "Clear Output"
        onClicked: bridge.clearOutput()
      }
      Item { Layout.fillWidth: true }
    }

    // ── Main area: code editor (left) | LBM widget panel (right) ─────────
    SplitView {
      Layout.fillWidth:  true
      Layout.fillHeight: true
      orientation: Qt.Horizontal

      // Code editor
      Rectangle {
        SplitView.preferredWidth: root.width * 0.58
        SplitView.fillHeight: true
        color: "#1e1e1e"
        border.color: "#555"
        border.width: 1

        ScrollView {
          anchors.fill: parent
          anchors.margins: 1
          TextArea {
            id: editor
            font.family:    "Monospace"
            font.pointSize: 11
            color:          "#d4d4d4"
            background:     Rectangle { color: "transparent" }
            placeholderText: "Enter LispBM code here..."
            wrapMode:       TextArea.NoWrap
            selectByMouse:  true
            tabStopDistance: 28
          }
        }
      }

      // LBM UI panel
      Rectangle {
        SplitView.fillWidth:  true
        SplitView.fillHeight: true
        color: palette.window
        border.color: "#aaa"
        border.width: 1

        ColumnLayout {
          anchors.fill: parent
          anchors.margins: 1
          spacing: 0

          Label {
            text: "LispBM UI"
            font.bold: true
            leftPadding: 6
            topPadding: 4
            bottomPadding: 4
            Layout.fillWidth: true
            background: Rectangle { color: palette.mid }
          }

          // This Item is the mount point: LispBM builds its widget tree here
          // via qt-widget-add-* extensions. objectName must stay "lbmArea"
          // so main.cpp can locate it by name after the engine loads.
          Item {
            id: lbmArea
            objectName: "lbmArea"
            Layout.fillWidth:  true
            Layout.fillHeight: true
            clip: true
          }
        }
      }
    }

    // ── Output ────────────────────────────────────────────────────────────
    Rectangle {
      Layout.fillWidth:      true
      Layout.preferredHeight: 160
      color:        "#1e1e1e"
      border.color: "#555"
      border.width: 1

      ScrollView {
        id: outputScroll
        anchors.fill: parent
        anchors.margins: 1

        TextArea {
          id: outputArea
          readOnly:        true
          font.family:     "Monospace"
          font.pointSize:  10
          color:           "#ccc"
          background:      Rectangle { color: "transparent" }
          placeholderText: "Output..."
          wrapMode:        TextArea.Wrap
          selectByMouse:   true
        }
      }

      Connections {
        target: bridge
        function onOutputAppended(text) {
          outputArea.insert(outputArea.length, text)
          outputArea.cursorPosition = outputArea.length
        }
        function onOutputCleared() {
          outputArea.clear()
        }
      }
    }
  }
}
