/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#ifndef CRC_H_
#define CRC_H_

#include <stdint.h>

/*
 * Functions
 */
unsigned short crc16(unsigned char *buf, unsigned int len);
uint32_t crc32(uint32_t *buf, uint32_t len);
void crc32_reset(void);
uint32_t crc32_with_init(uint8_t *buf, uint32_t len, uint32_t cksum);

#endif /* CRC_H_ */
