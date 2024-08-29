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

#ifdef __STRICT_ANSI__
#define isnanf isnan
#define isinff isinf
#endif

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


static lbm_value ext_is_nan(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1) {
    res = ENC_SYM_TERROR;
    if (lbm_is_number(args[0])) {
      lbm_uint t = lbm_type_of(args[0]);
      switch(t) {
      case LBM_TYPE_DOUBLE:
        if (isnan(lbm_dec_double(args[0]))) {
          res = ENC_SYM_TRUE;
        } else {
          res = ENC_SYM_NIL;
        }
        break;
      case LBM_TYPE_FLOAT:
        if (isnanf(lbm_dec_float(args[0]))) {
          res = ENC_SYM_TRUE;
        } else {
          res = ENC_SYM_NIL;
        }
        break;
      default:
        res = ENC_SYM_NIL;
        break;
      }     
    }
  }
  return res;
}

static lbm_value ext_is_inf(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1) {
    res = ENC_SYM_TERROR;
    if (lbm_is_number(args[0])) {
      lbm_uint t = lbm_type_of(args[0]);
      switch(t) {
      case LBM_TYPE_DOUBLE:
        if (isinf(lbm_dec_double(args[0]))) {
          res = ENC_SYM_TRUE;
        } else {
          res = ENC_SYM_NIL;
        }
        break;
      case LBM_TYPE_FLOAT:
        if (isinff(lbm_dec_float(args[0]))) {
          res = ENC_SYM_TRUE;
        } else {
          res = ENC_SYM_NIL;
        }
        break;
      default:
        res = ENC_SYM_NIL;
        break;
      }     
    }
  }
  return res;
}


void lbm_math_extensions_init(void) {

  lbm_add_extension("sin", ext_sin);
  lbm_add_extension("cos", ext_cos);
  lbm_add_extension("tan", ext_tan);
  lbm_add_extension("asin", ext_asin);
  lbm_add_extension("acos", ext_acos);
  lbm_add_extension("atan", ext_atan);
  lbm_add_extension("atan2", ext_atan2);
  lbm_add_extension("pow", ext_pow);
  lbm_add_extension("exp", ext_exp);
  lbm_add_extension("sqrt", ext_sqrt);
  lbm_add_extension("log", ext_log);
  lbm_add_extension("log10", ext_log10);
  lbm_add_extension("floor", ext_floor);
  lbm_add_extension("ceil", ext_ceil);
  lbm_add_extension("round", ext_round);
  lbm_add_extension("deg2rad", ext_deg2rad);
  lbm_add_extension("rad2deg", ext_rad2deg);
  lbm_add_extension("is-nan", ext_is_nan);
  lbm_add_extension("is-inf", ext_is_inf);
}
