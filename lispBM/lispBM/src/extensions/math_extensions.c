/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se
    Copyright 2022 Benjamin Vedder

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

#include "extensions.h"
#include "lbm_utils.h"

#include <math.h>

// Helpers

static bool is_number_all(lbm_value *args, lbm_uint argn) {
	for (lbm_uint i = 0;i < argn;i++) {
		if (!lbm_is_number(args[i])) {
			return false;
		}
	}
	return true;
}

#define CHECK_NUMBER_ALL()			if (!is_number_all(args, argn)) {return lbm_enc_sym(SYM_EERROR);}
#define CHECK_ARGN(n)				if (argn != n) {return lbm_enc_sym(SYM_EERROR);}
#define CHECK_ARGN_NUMBER(n)		if (argn != n || !is_number_all(args, argn)) {return lbm_enc_sym(SYM_EERROR);}


void rotate_vector3(float *input, float *rotation, float *output, bool reverse) {
  float s1, c1, s2, c2, s3, c3;

  if (rotation[2] != 0.0) {
    s1 = sinf(rotation[2]);
    c1 = cosf(rotation[2]);
  } else {
    s1 = 0.0;
    c1 = 1.0;
  }

  if (rotation[1] != 0.0) {
    s2 = sinf(rotation[1]);
    c2 = cosf(rotation[1]);
  } else {
    s2 = 0.0;
    c2 = 1.0;
  }

  if (rotation[0] != 0.0) {
    s3 = sinf(rotation[0]);
    c3 = cosf(rotation[0]);
  } else {
    s3 = 0.0;
    c3 = 1.0;
  }

  float m11 = c1 * c2;	float m12 = c1 * s2 * s3 - c3 * s1;	float m13 = s1 * s3 + c1 * c3 * s2;
  float m21 = c2 * s1;	float m22 = c1 * c3 + s1 * s2 * s3;	float m23 = c3 * s1 * s2 - c1 * s3;
  float m31 = -s2; 		float m32 = c2 * s3;				float m33 = c2 * c3;

  if (reverse) {
    output[0] = input[0] * m11 + input[1] * m21 + input[2] * m31;
    output[1] = input[0] * m12 + input[1] * m22 + input[2] * m32;
    output[2] = input[0] * m13 + input[1] * m23 + input[2] * m33;
  } else {
    output[0] = input[0] * m11 + input[1] * m12 + input[2] * m13;
    output[1] = input[0] * m21 + input[1] * m22 + input[2] * m23;
    output[2] = input[0] * m31 + input[1] * m32 + input[2] * m33;
  }
}

// Math

static lbm_value ext_sinf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(sinf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_cosf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(cosf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_tanf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(tanf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_asinf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(asinf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_acosf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(acosf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_atanf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(atanf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_atan2f(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(2)
    return lbm_enc_float(atan2f(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1])));
}

static lbm_value ext_powf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(2)
    return lbm_enc_float(powf(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1])));
}

static lbm_value ext_sqrtf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(sqrtf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_logf(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(logf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_log10f(lbm_value *args, lbm_uint argn) {
  CHECK_ARGN_NUMBER(1)
    return lbm_enc_float(log10f(lbm_dec_as_float(args[0])));
}

static lbm_value ext_deg2radf(lbm_value *args, lbm_uint argn) {
  CHECK_NUMBER_ALL();

  if (argn == 1) {
    return lbm_enc_float(DEG2RAD_f(lbm_dec_as_float(args[0])));
  } else {
    lbm_value out_list = lbm_enc_sym(SYM_NIL);
    for (int i = (int)(argn - 1);i >= 0;i--) {
      out_list = lbm_cons(lbm_enc_float(DEG2RAD_f(lbm_dec_as_float(args[i]))), out_list);
    }
    return out_list;
  }
}

static lbm_value ext_rad2degf(lbm_value *args, lbm_uint argn) {
  CHECK_NUMBER_ALL();

  if (argn == 1) {
    return lbm_enc_float(RAD2DEG_f(lbm_dec_as_float(args[0])));
  } else {
    lbm_value out_list = lbm_enc_sym(SYM_NIL);
    for (int i = (int)(argn - 1);i >= 0;i--) {
      out_list = lbm_cons(lbm_enc_float(RAD2DEG_f(lbm_dec_as_float(args[i]))), out_list);
    }
    return out_list;
  }
}

static lbm_value ext_vec3_rotf(lbm_value *args, lbm_uint argn) {
  CHECK_NUMBER_ALL();
  if (argn != 6 && argn != 7) {
    return lbm_enc_sym(SYM_EERROR);
  }

  float input[] = {lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1]), lbm_dec_as_float(args[2])};
  float rotation[] = {lbm_dec_as_float(args[3]), lbm_dec_as_float(args[4]), lbm_dec_as_float(args[5])};
  float output[3];

  bool reverse = false;
  if (argn == 7) {
    reverse = lbm_dec_as_i32(args[6]);
  }

  rotate_vector3(input, rotation, output, reverse);

  lbm_value out_list = lbm_enc_sym(SYM_NIL);
  out_list = lbm_cons(lbm_enc_float(output[2]), out_list);
  out_list = lbm_cons(lbm_enc_float(output[1]), out_list);
  out_list = lbm_cons(lbm_enc_float(output[0]), out_list);

  return out_list;
}

bool lbm_math_extensions_init(void) {

  bool res = true;
  res = res && lbm_add_extension("sin", ext_sinf);
  res = res && lbm_add_extension("cos", ext_cosf);
  res = res && lbm_add_extension("tan", ext_tanf);
  res = res && lbm_add_extension("asin", ext_asinf);
  res = res && lbm_add_extension("acos", ext_acosf);
  res = res && lbm_add_extension("atan", ext_atanf);
  res = res && lbm_add_extension("atan2", ext_atan2f);
  res = res && lbm_add_extension("pow", ext_powf);
  res = res && lbm_add_extension("sqrt", ext_sqrtf);
  res = res && lbm_add_extension("log", ext_logf);
  res = res && lbm_add_extension("log10", ext_log10f);
  res = res && lbm_add_extension("deg2rad", ext_deg2radf);
  res = res && lbm_add_extension("rad2deg", ext_rad2degf);
  res = res && lbm_add_extension("vec3-rot", ext_vec3_rotf);

  return res;
}
