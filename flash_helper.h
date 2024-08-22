/*
	Copyright 2016 - 2021 Benjamin Vedder	benjamin@vedder.se

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

#ifndef FLASH_HELPER_H_
#define FLASH_HELPER_H_

#include "conf_general.h"

#define CODE_IND_QML	0
#define CODE_IND_LISP	1

// Functions
uint16_t flash_helper_erase_new_app(uint32_t new_app_size);
uint16_t flash_helper_erase_bootloader(void);
uint16_t flash_helper_write_new_app_data(uint32_t offset, uint8_t *data, uint32_t len);

uint16_t flash_helper_erase_code(int ind);
uint16_t flash_helper_write_code(int ind, uint32_t offset, uint8_t *data, uint32_t len);
uint8_t* flash_helper_code_data(int ind);
uint8_t* flash_helper_code_data_raw(int ind);
uint32_t flash_helper_code_size(int ind);
uint16_t flash_helper_code_flags(int ind);

void flash_helper_jump_to_bootloader(void);
uint8_t* flash_helper_get_sector_address(uint32_t fsector);
uint32_t flash_helper_verify_flash_memory(void);
uint32_t flash_helper_verify_flash_memory_chunk(void);

// functions used in vesc_c_if.h and therefore accessible to packages
bool flash_helper_read_nvm(uint8_t *v, unsigned int len, unsigned int address);
bool flash_helper_write_nvm(uint8_t *v, unsigned int len, unsigned int address);
bool flash_helper_wipe_nvm(void);

#endif /* FLASH_HELPER_H_ */
