/*
	Copyright 2017 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HWCONF_TMC6200_H_
#define HWCONF_TMC6200_H_

#include "datatypes.h"

// Functions
bool tmc6200_ok(void);
void tmc6200_init(void);
//int tmc6200_read_faults(void);
void tmc6200_reset_faults(void);
void tmc6200_write_conf(void);



#endif /* HWCONF_TMC6200_H_ */
