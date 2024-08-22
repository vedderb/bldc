/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#include "vesc_c_if.h"

HEADER

static int abs(int x) {
	if (x >= 0) {
		return x;
	} else {
		return -x;
	}
}

static void set_pix(uint8_t *arr, int x, int y) {
	if (x < 0 || x >= 128 || y < 0 || y >= 64) {
		return;
	}
	unsigned int pos = 8 + x * 8 + y % 8 + (y / 8) * 1024;
	unsigned int bytepos = pos / 8;
	unsigned int bitpos = pos % 8;
	arr[bytepos] |= (1 << bitpos);
}

static void draw_line(uint8_t *arr, int x0, int y0, int x1, int y1) {
	int dx = abs(x1 - x0);
	int sx = x0 < x1 ? 1 : -1;
	int dy = -abs(y1 - y0);
	int sy = y0 < y1 ? 1 : -1;
	int error = dx + dy;
	
	while (true) {
		set_pix(arr, x0, y0);
		if (x0 == x1 && y0 == y1) {
			break;
		}
		if ((error * 2) >= dy) {
			if (x0 == x1) {
				break;
			}
			error += dy;
			x0 += sx;
		}
		if ((error * 2) <= dx) {
			if (y0 == y1) {
				break;
			}
			error += dx;
			y0 += sy;
		}
	}
}

static lbm_value ssd_drawline(lbm_value *args, lbm_uint argn) {
	lbm_value res = VESC_IF->lbm_enc_sym_eerror;

	if (argn != 5 || !VESC_IF->lbm_is_byte_array(args[0]) ||
			!VESC_IF->lbm_is_number(args[1]) || !VESC_IF->lbm_is_number(args[2]) ||
			!VESC_IF->lbm_is_number(args[3]) || !VESC_IF->lbm_is_number(args[4])) {
		return res;
	}
	
	uint8_t *pixbuf = (uint8_t*)VESC_IF->lbm_dec_str(args[0]);
	int x0 = VESC_IF->lbm_dec_as_i32(args[1]);
	int y0 = VESC_IF->lbm_dec_as_i32(args[2]);
	int x1 = VESC_IF->lbm_dec_as_i32(args[3]);
	int y1 = VESC_IF->lbm_dec_as_i32(args[4]);
	
	draw_line(pixbuf, x0, y0, x1, y1);
	
	return VESC_IF->lbm_enc_sym_true;
}

INIT_FUN(lib_info *info) {
	INIT_START

	(void)info;
	VESC_IF->lbm_add_extension("ext-drawline", ssd_drawline);
	return true;
}

