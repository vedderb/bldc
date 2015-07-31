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
 * buffer.h
 *
 *  Created on: 13 maj 2013
 *      Author: benjamin
 */

#ifndef BUFFER_H_
#define BUFFER_H_

#include <stdint.h>

void buffer_append_int16(uint8_t* buffer, int16_t number, int32_t *index);
void buffer_append_uint16(uint8_t* buffer, uint16_t number, int32_t *index);
void buffer_append_int32(uint8_t* buffer, int32_t number, int32_t *index);
void buffer_append_uint32(uint8_t* buffer, uint32_t number, int32_t *index);
void buffer_append_float16(uint8_t* buffer, float number, float scale, int32_t *index);
void buffer_append_float32(uint8_t* buffer, float number, float scale, int32_t *index);
int16_t buffer_get_int16(const uint8_t *buffer, int32_t *index);
uint16_t buffer_get_uint16(const uint8_t *buffer, int32_t *index);
int32_t buffer_get_int32(const uint8_t *buffer, int32_t *index);
uint32_t buffer_get_uint32(const uint8_t *buffer, int32_t *index);
float buffer_get_float16(const uint8_t *buffer, float scale, int32_t *index);
float buffer_get_float32(const uint8_t *buffer, float scale, int32_t *index);

#endif /* BUFFER_H_ */
