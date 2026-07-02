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

#ifndef REPL_QT_H_
#define REPL_QT_H_

#ifdef __cplusplus
extern "C" {
#endif

/* Create QGuiApplication + Qt Quick window, register qt-* extensions, run
   app.exec().  Returns when the window is closed.  If the user types :quit
   in the REPL thread, terminate_repl() calls exit() first so this may never
   return. */
void repl_qt_run(int argc, char **argv);

#ifdef __cplusplus
}
#endif

#endif
