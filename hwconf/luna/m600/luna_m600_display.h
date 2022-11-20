/*
	Copyright 2020 Marcos Chaparro	mchaparro@powerdesigns.ca

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

#ifndef APP_LUNA_DISPLAY_CANBUS_H_
#define APP_LUNA_DISPLAY_CANBUS_H_

#include "stdint.h"

void luna_canbus_start(void);
float luna_canbus_get_PAS_torque(void);
float get_encoder_error(void);
#endif /* APP_LUNA_DISPLAY_CANBUS_H_ */
