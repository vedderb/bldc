/*
    Copyright 2026 Joel Svensson        svenssonjoel@yahoo.se

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
#include "lbm_c_interop.h"
#include "ecc_extensions.h"
#include "ecc.h"

#ifdef LBM_OPT_ECC_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_ECC_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

static lbm_value ext_rs_encoded_size(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {
    int payload_size = (int)lbm_dec_as_i32(args[0]);
    int nroots       = (int)lbm_dec_as_i32(args[1]);
    if (payload_size <= 0 || nroots < 2 || nroots % 2 != 0 ||
        payload_size + nroots > 255) {
      res = ENC_SYM_EERROR;
    } else {
      res = lbm_enc_i(ecc_rs_encoded_size(payload_size, nroots));
    }
  }
  return res;
}

static lbm_value ext_rs_encode(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_number(args[1])) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(args[0]);
    int nroots      = (int)lbm_dec_as_i32(args[1]);
    int total       = (int)arr->size;
    int payload_len = total - nroots;
    if (payload_len <= 0 || nroots < 2 || nroots > RS_MAX_ROOTS || nroots % 2 != 0 || total > 255) {
      res = ENC_SYM_EERROR;
    } else {
      ecc_rs_encode((uint8_t*)arr->data, payload_len, nroots);
      res = args[0];
    }
  }
  return res;
}

static lbm_value ext_rs_decode(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_number(args[1])) {
    lbm_array_header_t *arr     = (lbm_array_header_t*)lbm_car(args[0]);
    int                 nroots  = (int)lbm_dec_as_i32(args[1]);
    int                 total   = (int)arr->size;
    int                 payload = total - nroots;
    if (payload <= 0 || nroots < 2 || nroots > RS_MAX_ROOTS || nroots % 2 != 0 || total > 255) {
      res = ENC_SYM_EERROR;
    } else {
      res = lbm_enc_i(ecc_rs_decode((uint8_t*)arr->data, payload, nroots));
    }
  }
  return res;
}

void lbm_ecc_extensions_init(void) {
  lbm_add_extension("rs-encode",       ext_rs_encode);
  lbm_add_extension("rs-decode",       ext_rs_decode);
  lbm_add_extension("rs-encoded-size", ext_rs_encoded_size);
}
