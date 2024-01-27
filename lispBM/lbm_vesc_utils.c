/*
	Copyright 2023 Rasmus Söderhielm    rasmus.soderhielm@gmail.com

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

#include <stdbool.h>

#include "symrepr.h"
#include "lbm_defines.h"
#include "lbm_types.h"
#include "heap.h"
#include "eval_cps.h"
#include "lbm_flat_value.h"
#include "commands.h"

#include "lbm_vesc_utils.h"

bool lbm_add_symbol_const_if_new(char *name, lbm_uint *id) {
	if (!lbm_get_symbol_by_name(name, id) && !lbm_add_symbol_const(name, id)) {
		return false;
	}

	return true;
}

lbm_array_header_t *lbm_dec_array_header(lbm_value value) {
	if (!lbm_is_array_r(value)) {
		return NULL;
	}

	return (lbm_array_header_t *)lbm_car(value);
}

void *lbm_dec_array_data(lbm_value value) {
	if (!lbm_is_array_r(value)) {
		return NULL;
	}

	lbm_array_header_t *header = lbm_dec_array_header(value);
	if (!header->data) {
		return NULL;
	}
	return header->data;
}

lbm_value lbm_allocate_empty_list(lbm_uint len) {
	lbm_value res = ENC_SYM_NIL;
	for (lbm_uint i = 0; i < len; i++) {
		res = lbm_cons(ENC_SYM_NIL, res);
		if (res == ENC_SYM_MERROR) {
			return ENC_SYM_MERROR;
		}
	}
	return res;
}

lbm_value lbm_allocate_empty_list_grid(lbm_uint height, lbm_uint width) {
	lbm_value outer = ENC_SYM_NIL;
	for (lbm_uint i = 0; i < height; i++) {
		lbm_value inner = ENC_SYM_NIL;
		for (lbm_uint j = 0; j < width; j++) {
			inner = lbm_cons(ENC_SYM_NIL, inner);
			if (inner == ENC_SYM_MERROR) {
				return ENC_SYM_MERROR;
			}
		}

		outer = lbm_cons(inner, outer);
		if (outer == ENC_SYM_MERROR) {
			return ENC_SYM_MERROR;
		}
	}

	return outer;
}

bool lbm_memory_shrink_bytes(void *array, lbm_uint size_bytes) {
	lbm_uint size_words = size_bytes / LBM_WORD_SIZE;
	if (size_bytes % LBM_WORD_SIZE != 0) {
		size_words += 1;
	}

	return lbm_memory_shrink((lbm_uint *)array, size_words) > 0;
}

bool lbm_array_shrink(lbm_value array, lbm_uint new_size) {
	if (!lbm_is_array_rw(array)) {
		return false;
	}

	lbm_array_header_t *header = (lbm_array_header_t *)lbm_car(array);

	if (!lbm_memory_shrink_bytes(header->data, new_size)) {
		return false;
	}
	header->size = new_size;

	return true;
}

extern const char *lbm_error_str_num_args;
bool lbm_check_argn_range(lbm_uint argn, lbm_uint n_min, lbm_uint n_max) {
	if (!(n_min <= argn && argn <= n_max)) {
		lbm_set_error_reason((char *)lbm_error_str_num_args);
		return false;
	}

	return true;
}

bool lbm_check_argn_least(lbm_uint argn, lbm_uint n_min) {
	if (!(n_min <= argn)) {
		lbm_set_error_reason((char *)lbm_error_str_num_args);
		return false;
	}

	return true;
}

bool f_pack_array(lbm_flat_value_t *result, void *data, size_t size) {
	if (!lbm_start_flatten(result, 5 + size)) {
		return false;
	}

	if (!f_lbm_array(result, size, data)) {
		return false;
	}

	if (!lbm_finish_flatten(result)) {
		return false;
	}

	return true;
}