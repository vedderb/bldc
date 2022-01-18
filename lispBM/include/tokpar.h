/*
    Copyright 2019 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef TOKPAR_H_
#define TOKPAR_H_

#include "lispbm_types.h"

// TODO: Include the following two lines in some platform header, which then is included here
#include "utils.h"
#define TOKPAR_CHECK_STACK()	(utils_stack_left_now() > 350)

VALUE tokpar_parse(char *str);
VALUE tokpar_parse_stream(
    bool (*more)(void),
    char (*get)(void),
    char (*peek)(unsigned int n),
    void (*drop)(unsigned int n));

#endif
