/*
    Copyright 2023 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef LOOP_EXTENSIONS_H_
#define LOOP_EXTENSIONS_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

extern const char *loop_extensions_dyn_load[4];

bool lbm_loop_extensions_init(void);

#ifdef __cplusplus
}
#endif
#endif
