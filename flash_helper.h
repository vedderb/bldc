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
 * flash_helper.h
 *
 *  Created on: 6 maj 2015
 *      Author: benjamin
 */

#ifndef FLASH_HELPER_H_
#define FLASH_HELPER_H_

#include "conf_general.h"

// Functions
uint16_t flash_helper_erase_new_app(uint32_t new_app_size);
uint16_t flash_helper_write_new_app_data(uint32_t offset, uint8_t *data, uint32_t len);
void flash_helper_jump_to_bootloader(void);
uint8_t* flash_helper_get_sector_address(uint32_t fsector);

#endif /* FLASH_HELPER_H_ */
