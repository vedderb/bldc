/*
	Copyright 2020 Benjamin Vedder	benjamin@vedder.se

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

#ifndef MEMPOOLS_H_
#define MEMPOOLS_H_

#include "datatypes.h"

// Settings
#define MEMPOOLS_MCCONF_NUM				10
#define MEMPOOLS_APPCONF_NUM			3

// Functions
mc_configuration *mempools_alloc_mcconf(void);
void mempools_free_mcconf(mc_configuration *conf);

app_configuration *mempools_alloc_appconf(void);
void mempools_free_appconf(app_configuration *conf);

int mempools_mcconf_highest(void);
int mempools_appconf_highest(void);

int mempools_mcconf_allocated_num(void);
int mempools_appconf_allocated_num(void);

#endif /* MEMPOOLS_H_ */
