/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

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

/*
 * terminal.h
 *
 *  Created on: 26 dec 2013
 *      Author: benjamin
 */

#ifndef TERMINAL_H_
#define TERMINAL_H_

#include "datatypes.h"

// Functions
void terminal_process_string(char *str);
void terminal_add_fault_data(fault_data *data);

#endif /* TERMINAL_H_ */
