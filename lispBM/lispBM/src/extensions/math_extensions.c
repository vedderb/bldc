/*
    Copyright 2022, 2023 Joel Svensson        svenssonjoel@yahoo.se
    Copyright 2022, 2023 Benjamin Vedder

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

// Math

static lbm_value ext_sin(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(sinf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_cos(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(cosf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_tan(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(tanf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_asin(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(asinf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_acos(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(acosf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_atan(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(atanf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_atan2(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(2);
  return lbm_enc_float(atan2f(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1])));
}

static lbm_value ext_pow(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(2);
  return lbm_enc_float(powf(lbm_dec_as_float(args[0]), lbm_dec_as_float(args[1])));
}

static lbm_value ext_exp(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(expf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_sqrt(lbm_value *args, lbm_uint argn) {
    LBM_CHECK_ARGN_NUMBER(1);
    return lbm_enc_float(sqrtf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_log(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(logf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_log10(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(log10f(lbm_dec_as_float(args[0])));
}

static lbm_value ext_floor(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(floorf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_ceil(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(ceilf(lbm_dec_as_float(args[0])));
}

static lbm_value ext_round(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  return lbm_enc_float(roundf(lbm_dec_as_float(args[0])));
}

 static lbm_value ext_deg2rad(lbm_value *args, lbm_uint argn) {
   LBM_CHECK_NUMBER_ALL();

   if (argn == 1) {
     return lbm_enc_float(DEG2RAD_f(lbm_dec_as_float(args[0])));
   } else {
     lbm_value out_list = ENC_SYM_NIL;
     for (int i = (int)(argn - 1);i >= 0;i--) {
       out_list = lbm_cons(lbm_enc_float(DEG2RAD_f(lbm_dec_as_float(args[i]))), out_list);
     }
     return out_list;
   }
 }

 static lbm_value ext_rad2deg(lbm_value *args, lbm_uint argn) {
   LBM_CHECK_NUMBER_ALL();

   if (argn == 1) {
     return lbm_enc_float(RAD2DEG_f(lbm_dec_as_float(args[0])));
   } else {
     lbm_value out_list = ENC_SYM_NIL;
     for (int i = (int)(argn - 1);i >= 0;i--) {
       out_list = lbm_cons(lbm_enc_float(RAD2DEG_f(lbm_dec_as_float(args[i]))), out_list);
     }
     return out_list;
   }
 }

bool lbm_math_extensions_init(void) {

  bool res = true;
  res = res && lbm_add_extension("sin", ext_sin);
  res = res && lbm_add_extension("cos", ext_cos);
  res = res && lbm_add_extension("tan", ext_tan);
  res = res && lbm_add_extension("asin", ext_asin);
  res = res && lbm_add_extension("acos", ext_acos);
  res = res && lbm_add_extension("atan", ext_atan);
  res = res && lbm_add_extension("atan2", ext_atan2);
  res = res && lbm_add_extension("pow", ext_pow);
  res = res && lbm_add_extension("exp", ext_exp);
  res = res && lbm_add_extension("sqrt", ext_sqrt);
  res = res && lbm_add_extension("log", ext_log);
  res = res && lbm_add_extension("log10", ext_log10);
  res = res && lbm_add_extension("floor", ext_floor);
  res = res && lbm_add_extension("ceil", ext_ceil);
  res = res && lbm_add_extension("round", ext_round);
  res = res && lbm_add_extension("deg2rad", ext_deg2rad);
  res = res && lbm_add_extension("rad2deg", ext_rad2deg);
  return res;
}
