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

#include "lbm_types.h"
#include "lbm_defines.h"
#include "heap.h"
#include "lbm_flat_value.h"

// Is this the right place to define this?
/**
 * Bytes per word in the LBM memory.
*/
#define LBM_WORD_SIZE 4

/**
 * Extract the array header struct from a lbm value array.
 * 
 * The type of the value is checked to be at least be a readable array (doesn't
 * have to be writeable though).
 * 
 * @param value The lbm value to convert to an array header struct.
 * @return The extracted pointer to the array header struct if value was a
 * readable array. Null is returned otherwise. (@note: Null is also returned if
 * the lbm_value contained a null pointer, @todo: unsure exactly what layer that would
 * be right now though...)
*/
lbm_array_header_t *lbm_dec_array_header(lbm_value value);

/**
 * Wrapper around lbm_memory_shrink that takes number of bytes instead of number
 * of words. Shrinks the size of an pointer allocated in LBM memory to the
 * smallest possible size while still having capacity for the specified amount
 * of bytes.
 * 
 * @param ptr Pointer to the allocated segment in LBM memory. Should have been
 * obtained through lbm_malloc or other similar way at some point.
 * @param size_bytes The new capacity of the allocation in bytes. Must be
 * smaller or equal to the previous capacity.
 * @return If the operation succeeded. The return value of lbm_memory_shrink is
 * directly passed through, that is: false is returned either if ptr didn't
 * point into the LBM memory/didn't point to the start of an allocated segment
 * or if the new size was larger than the previous (note that since this
 * function converts bytes to words, a larger size in bytes might not cause it
 * to fail, as the size in words could still be the same). Otherwise true is
 * returned. 
*/
bool lbm_memory_shrink_bytes(void *ptr, lbm_uint size_bytes);

/**
 * Shrink an lbm array to the new specified size.
 * 
 * @param lbm_value The array to shrink. Should hold an LBM value that
 * corresponds to an array.
 * @param new_size The new smaller array size.
 * @return Bool indicating if the array was successfully shrunk. False is
 * returned if array didn't hold a byte array value, or if new_len was larger
 * than the original size.
*/
bool lbm_array_shrink(lbm_value array, lbm_uint new_size);

/**
 * Check if the number of arguments is the specified range. Sets error-reason if
 * result is false.
 * 
 * The range specified is inclusive!
 * 
 * @param argn Number of arguments.
 * @param n_min Minimum number of arguments.
 * @param n_max Maximum number of arguments.
 * 
*/
bool lbm_check_argn_range(lbm_uint argn, lbm_uint n_min, lbm_uint n_max);

/**
 * Check if the number of arguments is at large as long as specified. Sets
 * error-reason if result is false.
 * 
 * The (open) range specified is inclusive!
 * 
 * @param argn Number of arguments.
 * @param n_min Minimum number of arguments.
 * 
*/
bool lbm_check_argn_least(lbm_uint argn, lbm_uint n_min);

#define LBM_CHECK_ARGN_RANGE(min, max)                                         \
	if (!lbm_check_argn_range(argn, (min), (max))) {                           \
		return ENC_SYM_EERROR;                                                 \
	}
#define LBM_CHECK_ARGN_LEAST(min)                                              \
	if (!lbm_check_argn_least(argn, (min))) {                                  \
		return ENC_SYM_EERROR;                                                 \
	}
