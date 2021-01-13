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

#ifndef SI8900_H_
#define SI8900_H_

#include "ch.h"
#include "hal.h"

// Functions
void si8900_init(void);
float si8900_get_voltage(int channel);
float si8900_get_val_rel(int channel);

// Commands
#define SI8900_CNFG_0			0xC0
#define SI8900_CNFG_0_PGA		(1 << 0)
#define SI8900_CNFG_0_MODE		(1 << 1)
#define SI8900_CNFG_0_VREF		(1 << 3)
#define SI8900_CNFG_0_MX0		(1 << 4)
#define SI8900_CNFG_0_MX1		(1 << 5)

#endif /* SI8900_H_ */
