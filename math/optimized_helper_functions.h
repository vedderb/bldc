/*
	Copyright 2021 Kenn Sebesta

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

#ifndef OPTIMIZED_HELPER_FUNCTIONS_H_
#define OPTIMIZED_HELPER_FUNCTIONS_H_

#include <stdbool.h>
#include <stdint.h>
#include "datatypes.h"
#include <math.h>
#include <stdlib.h>

// This takes advantage of the IEEE 754 standard, which requires that the sign
// bit of single-precision floats is the last bit.
// It is faster than `signbit()` because it does not have a bit shift and does no type checking.
// On the flip side, it is dangerous because it does no checking of any kind. It will
// not care if the input is not a `float`, if it is `NaN`, etc...
// It is guaranteed to work on all systems which adhere to IEEE 754.
// This works on both big- and little-endian systems.
// The macro returns 1 if a `float` is negative, 0 if otherwise
#define IS_FLOAT_IEEE754_NEGATIVE(x) (*(uint32_t *)(&(x)) & (1U<<31))


/**
 * @brief calculate_mod_alpha_beta_filter_sign Exploits the fact that two of the three
 * currents will always have the same sign, and the third will always have the opposite sign.
 * @param mod_alpha_filter_sgn Returns a value on the set of [-4/3, -2/3, 2/3, 4/3]
 * @param mod_beta_filter_sgn Returns a value on the set of [-2/sqrt(3), 0, 2/sqrt(3)]
 * @param ia_filter The filtered i_a
 * @param ib_filter The filtered i_b
 * @param ic_filter The filtered i_c
 */
inline __attribute__((always_inline)) void calculate_mod_alpha_beta_filter_sign(float *mod_alpha_filter_sgn, float *mod_beta_filter_sgn, float ia_filter, float ib_filter, float ic_filter) {
	// This calculates the below code, which is effectively a truth table with floats as outputs:
	//    mod_alpha_filter_sgn = (1.0 / 3.0) * (2 * SIGN(ia_filter) - SIGN(ib_filter) - SIGN(ic_filter));
	//    mod_beta_filter_sgn = ONE_BY_SQRT3 * (SIGN(ib_filter) - SIGN(ic_filter));

	/* Perform a compile-time assert, checking that a float and a uint32_t are the same size on this machine. */
	_Static_assert(sizeof(float) == sizeof(uint32_t), "This compilation is not compatible. Please fix the IEEE754 relationship between floats and uints");

#pragma GCC diagnostic push  // Stores any GCC directives
#pragma GCC diagnostic ignored "-Wstrict-aliasing"  // Disables the strict-aliasing directive. This is necessary for the IS_FLOAT_IEEE754_NEGATIVE() macro.

	// Check if both `ib_filter` and `ic_filter` have the same sign.
	if (IS_FLOAT_IEEE754_NEGATIVE(ib_filter) == IS_FLOAT_IEEE754_NEGATIVE(ic_filter)) {
		// Both `ib_filter` and `ic_filter` have the same sign, so they cancel out
		*mod_beta_filter_sgn = 0;

		// Check if `ia_filter` is negative
		if (IS_FLOAT_IEEE754_NEGATIVE(ia_filter)) {
			// ia_filter is negative, which means both `ib_filter` and `ic_filter` are positive.
			*mod_alpha_filter_sgn = -4.0 / 3.0;
		} else {
			// ia_filter is positive, which means both `ib_filter` and `ic_filter` are negative.
			*mod_alpha_filter_sgn =  4.0 / 3.0;
		}
	} else  {
		// `ib_filter` and `ic_filter` have opposite signs

		if (IS_FLOAT_IEEE754_NEGATIVE(ia_filter)) {
			// ia_filter is negative. `ib_filter` and `ic_filter` have opposite signs so they cancel out.
			*mod_alpha_filter_sgn = -2.0 / 3.0;
		} else {
			// ia_filter is positive. `ib_filter` and `ic_filter` have opposite signs so they cancel out.
			*mod_alpha_filter_sgn =  2.0 / 3.0;
		}

		if (IS_FLOAT_IEEE754_NEGATIVE(ib_filter)) {
			// ib_filter is negative, ic_filter is positive
			*mod_beta_filter_sgn = -TWO_BY_SQRT3;
		} else {
			// ib_filter is positive, ic_filter is negative
			*mod_beta_filter_sgn =  TWO_BY_SQRT3;
		}
	}
#pragma GCC diagnostic pop  // This restores the GCC directives
}

#endif  // OPTIMIZED_HELPER_FUNCTIONS_H_
