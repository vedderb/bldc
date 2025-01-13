/*
    Copyright 2023, 2025 Joel Svensson        svenssonjoel@yahoo.se
              2022       Benjamin Vedder      benjamin@vedder.se

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

#ifndef LBM_LOOP_EXTENSIONS_H_
#define LBM_LOOP_EXTENSIONS_H_

const char *loop_extensions_dyn_load[] =
  {
   "(define loopfor (macro (it start cnd update body) (me-loopfor it start cnd update body)))",
   "(define loopwhile (macro (cnd body) (me-loopwhile cnd body)))",
   "(define looprange (macro (it start end body) (me-looprange it start end body)))",
   "(define loopforeach (macro (it lst body) (me-loopforeach it lst body)))",
   "(define loopwhile-thd (macro (stk cnd body) `(spawn ,stk (fn () (loopwhile ,cnd ,body)))))",
  };

void lbm_loop_extensions_init(void);

#endif
