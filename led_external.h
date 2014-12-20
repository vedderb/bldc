/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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
 * led_external.h
 *
 *  Created on: 5 dec 2014
 *      Author: benjamin
 */

#ifndef LED_EXTERNAL_H_
#define LED_EXTERNAL_H_

#include "datatypes.h"

// Functions
void led_external_init(void);
void led_external_set_state(LED_EXT_STATE new_state);
void led_external_set_reversed(bool newstate);

#endif /* LED_EXTERNAL_H_ */
