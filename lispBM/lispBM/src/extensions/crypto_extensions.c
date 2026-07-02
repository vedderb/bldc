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
#include "crypto_extensions.h"
#include "crypto.h"
#include <string.h>

#ifdef LBM_OPT_CRYPTO_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_CRYPTO_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

static lbm_value ext_sha256_str(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    if(lbm_create_array(&res, 32)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
      char *str = (char*)in_arr->data;
      uint32_t n = (uint32_t)strlen_max(str, in_arr->size);
      crypto_sha256((uint8_t*)arr->data, (uint8_t*)str, n);
    }
  }
  return res;
}

static lbm_value ext_sha256(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    if(lbm_create_array(&res, 32)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
      crypto_sha256((uint8_t*)arr->data, (uint8_t*)in_arr->data, (uint32_t)in_arr->size);
    }
  }
  return res;
}



static lbm_value ext_aes128_enc(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_array_header_t *key  = lbm_dec_array_r(args[0]);
    lbm_array_header_t *data = lbm_dec_array_r(args[1]);

    // encodes one block of 16 bytes.
    if (key->size == 16 && data->size == 16) {
      uint8_t key_expanded[176];
      crypto_aes_key_expansion((uint8_t*)key->data, key_expanded, CRYPTO_AES128_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        crypto_aes_cipher((uint8_t*)out->data, key_expanded, CRYPTO_AES128_NUM_ROUNDS);
      }
    } else {
      res = ENC_SYM_EERROR;
    }
  }
  return res;
}

static lbm_value ext_aes128_dec(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_array_header_t *key  = lbm_dec_array_r(args[0]);
    lbm_array_header_t *data = lbm_dec_array_r(args[1]);

    // decodes one block of 16 bytes.
    if (key->size == 16 && data->size == 16) {
      uint8_t key_expanded[176];
      crypto_aes_key_expansion((uint8_t*)key->data, key_expanded, CRYPTO_AES128_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        crypto_aes_inv_cipher((uint8_t*)out->data, key_expanded, CRYPTO_AES128_NUM_ROUNDS);
      }
    } else {
      res = ENC_SYM_EERROR;
    }
  }
  return res;
}


static lbm_value ext_aes256_enc(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_array_header_t *key  = lbm_dec_array_r(args[0]);
    lbm_array_header_t *data = lbm_dec_array_r(args[1]);

    // encodes one block of 16 bytes.
    if (key->size == 32 && data->size == 16) {
      uint8_t key_expanded[240];
      crypto_aes_key_expansion((uint8_t*)key->data, key_expanded, CRYPTO_AES256_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        crypto_aes_cipher((uint8_t*)out->data, key_expanded, CRYPTO_AES256_NUM_ROUNDS);
      }
    } else {
      res = ENC_SYM_EERROR;
    }
  }
  return res;
}

static lbm_value ext_aes256_dec(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_array_header_t *key  = lbm_dec_array_r(args[0]);
    lbm_array_header_t *data = lbm_dec_array_r(args[1]);

    // decodes one block of 16 bytes.
    if (key->size == 32 && data->size == 16) {
      uint8_t key_expanded[240];
      crypto_aes_key_expansion((uint8_t*)key->data, key_expanded, CRYPTO_AES256_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        crypto_aes_inv_cipher((uint8_t*)out->data, key_expanded, CRYPTO_AES256_NUM_ROUNDS);
      }
    } else {
      res = ENC_SYM_EERROR;
    }
  }
  return res;
}

// ////////////////////////////////////////////////////////////
// Utilities
//

static lbm_value ext_bytes_to_hex(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
    if (lbm_create_array(&res, in_arr->size * 2 + 1)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      crypto_bytes_to_hex((uint8_t*)in_arr->data, (uint32_t)in_arr->size, (char*)arr->data);
    }
  }
  return res;
}

static lbm_value ext_hex_to_bytes(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
    res = ENC_SYM_EERROR;
    if (in_arr->size % 2 == 1) { // null-terminated: size odd means even number of hex chars
      lbm_uint bytes_len = (in_arr->size - 1) / 2;
      if (lbm_create_array(&res, bytes_len)) {
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
        crypto_hex_to_bytes((uint8_t*)in_arr->data, (uint32_t)(in_arr->size - 1), (uint8_t*)arr->data);
      }
    }
  }
  return res;
}

// ////////////////////////////////////////////////////////////
// Bignum
//

static lbm_value ext_bn_divmod(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = crypto_bn_normalize_len(ap, (uint32_t)(a->size / sizeof(uint32_t)));
    uint32_t blen = crypto_bn_normalize_len(bp, (uint32_t)(b->size / sizeof(uint32_t)));

    if (crypto_bn_cmp(ap, alen, bp, blen) < 0) {
      // a < b: q = 0, r = a
      lbm_value qv, rv;
      if (lbm_heap_allocate_array(&qv, sizeof(uint32_t)) &&
          lbm_heap_allocate_array(&rv, alen * sizeof(uint32_t))) {
        ((uint32_t*)lbm_dec_array_r(qv)->data)[0] = 0;
        memcpy(lbm_dec_array_r(rv)->data, ap, alen * sizeof(uint32_t));
        res = lbm_cons(qv, rv);
      } else {
        res = ENC_SYM_MERROR;
      }
    } else if (blen == 1) {
      lbm_value qv, rv;
      if (lbm_heap_allocate_array(&qv, alen * sizeof(uint32_t)) &&
          lbm_heap_allocate_array(&rv, sizeof(uint32_t))) {
        crypto_bn_divmod_single(ap, alen, bp[0],
                         (uint32_t*)lbm_dec_array_r(qv)->data,
                         (uint32_t*)lbm_dec_array_r(rv)->data);
        res = lbm_cons(qv, rv);
      } else {
        res = ENC_SYM_MERROR;
      }
    } else {
      // General case: allocate q, r, and scratch
      lbm_value qv, rv, s_an, s_bn;
      if (lbm_heap_allocate_array(&qv,   alen * sizeof(uint32_t)) &&
          lbm_heap_allocate_array(&rv,   blen * sizeof(uint32_t)) &&
          lbm_heap_allocate_array(&s_an, (alen + 1) * sizeof(uint32_t)) &&
          lbm_heap_allocate_array(&s_bn, blen * sizeof(uint32_t))) {
        crypto_bn_divmod(ap, alen, bp, blen,
                  (uint32_t*)lbm_dec_array_r(qv)->data,
                  (uint32_t*)lbm_dec_array_r(rv)->data,
                  (uint32_t*)lbm_dec_array_r(s_an)->data,
                  (uint32_t*)lbm_dec_array_r(s_bn)->data);
        res = lbm_cons(qv, rv);
      } else {
        res = ENC_SYM_MERROR;
      }
    }
  }
  return res;
}

// ////////////////////////////////////////////////////////////
// Extensions

static lbm_value ext_bn_cmp(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = crypto_bn_normalize_len(ap, (uint32_t)(a->size / sizeof(uint32_t)));
    uint32_t blen = crypto_bn_normalize_len(bp, (uint32_t)(b->size / sizeof(uint32_t)));
    r = lbm_enc_i(crypto_bn_cmp(ap, alen, bp, blen));
  }
  return r;
}

static lbm_value ext_bn_add(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = crypto_bn_normalize_len(ap, (uint32_t)(a->size / sizeof(uint32_t)));
    uint32_t blen = crypto_bn_normalize_len(bp, (uint32_t)(b->size / sizeof(uint32_t)));
    if (alen < blen) { // ensure alen >= blen
      uint32_t *tmp = ap; ap = bp; bp = tmp;
      uint32_t  tl  = alen; alen = blen; blen = tl;
    }
    lbm_value res;
    if (lbm_heap_allocate_array(&res, (alen + 1) * sizeof(uint32_t))) {
      lbm_array_header_t *arr = lbm_dec_array_r(res);
      crypto_bn_add(ap, alen, bp, blen, (uint32_t*)arr->data);
      r = res;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

static lbm_value ext_bn_sub(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = crypto_bn_normalize_len(ap, (uint32_t)(a->size / sizeof(uint32_t)));
    uint32_t blen = crypto_bn_normalize_len(bp, (uint32_t)(b->size / sizeof(uint32_t)));
    r = ENC_SYM_EERROR;
    if (crypto_bn_cmp(ap, alen, bp, blen) >= 0) { // a >= b required
      lbm_value res;
      if (lbm_heap_allocate_array(&res, alen * sizeof(uint32_t))) {
        lbm_array_header_t *arr = lbm_dec_array_r(res);
        crypto_bn_sub(ap, alen, bp, blen, (uint32_t*)arr->data);
        r = res;
      } else {
        r = ENC_SYM_MERROR;
      }
    }
  }
  return r;
}

static lbm_value ext_bn_mul(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = crypto_bn_normalize_len(ap, (uint32_t)(a->size / sizeof(uint32_t)));
    uint32_t blen = crypto_bn_normalize_len(bp, (uint32_t)(b->size / sizeof(uint32_t)));
    lbm_value res;
    if (lbm_heap_allocate_array(&res, (alen + blen) * sizeof(uint32_t))) {
      lbm_array_header_t *arr = lbm_dec_array_r(res);
      crypto_bn_mul(ap, alen, bp, blen, (uint32_t*)arr->data);
      r = res;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

// Convert bignum to u32. Returns EERROR if bignum is too large to fit.
static lbm_value ext_bn_to_u32(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *arr = lbm_dec_array_r(args[0]);
    uint32_t *ap = (uint32_t*)arr->data;
    uint32_t alen = crypto_bn_normalize_len(ap, (uint32_t)(arr->size / sizeof(uint32_t)));
    if (alen == 1) {
      r = lbm_enc_u32(ap[0]);
    } else {
      r = ENC_SYM_EERROR;
    }
  }
  return r;
}

//convert u32 to bignum
static lbm_value ext_bn_from_u32(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_value bn;
    if (lbm_heap_allocate_array(&bn, sizeof(uint32_t))) {
      // dec_array_r is a bit misleading as we are writing.
      // But it is ok and the _r variant should be slightly cheaper.
      // We know it is writable because we just allocated it!
      lbm_array_header_t *arr = lbm_dec_array_r(bn);
      uint32_t *vp = (uint32_t*)&arr->data[0];
      *vp = lbm_dec_as_u32(args[0]);
      r = bn;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

static lbm_value ext_bn_from_bytes(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *byte_array = lbm_dec_array_r(args[0]);
    r = ENC_SYM_EERROR;
    if (byte_array->size % sizeof(uint32_t) == 0) {
      uint32_t num_bytes = (uint32_t)byte_array->size;
      lbm_value bn ;
      if (lbm_heap_allocate_array(&bn, num_bytes)) {
        lbm_array_header_t *arr = lbm_dec_array_r(bn);
        for (uint32_t i = 0; i < num_bytes ; i ++) {
          ((uint8_t*)arr->data)[i] = ((uint8_t*)byte_array->data)[num_bytes-i-1];
        }
        r = bn;
      } else {
        r = ENC_SYM_MERROR;
      }
    }
  }
  return r;
}


static lbm_value ext_bn_to_bytes(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *bn = lbm_dec_array_r(args[0]);
    uint32_t num_bytes = (uint32_t)bn->size;
    lbm_value byte_array;
    if (lbm_heap_allocate_array(&byte_array, num_bytes)) {
      lbm_array_header_t *arr = lbm_dec_array_r(byte_array);
      for (uint32_t i = 0; i < num_bytes; i++) {
        ((uint8_t*)arr->data)[i] = ((uint8_t*)bn->data)[num_bytes - i - 1];
      }
      r = byte_array;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

// Allocates a bunch of memory which is bad from
// a fragmentation point of view.
// Possibly look at making defrag_mem an option.
static lbm_value ext_bn_modexp(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 3 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1]) &&
      lbm_is_array_r(args[2])) {
    lbm_array_header_t *base_arr = lbm_dec_array_r(args[0]);
    lbm_array_header_t *exp_arr  = lbm_dec_array_r(args[1]);
    lbm_array_header_t *n_arr    = lbm_dec_array_r(args[2]);
    uint32_t *base = (uint32_t*)base_arr->data;
    uint32_t *exp  = (uint32_t*)exp_arr->data;
    uint32_t *n    = (uint32_t*)n_arr->data;
    uint32_t blen  = crypto_bn_normalize_len(base, (uint32_t)(base_arr->size / sizeof(uint32_t)));
    uint32_t elen  = crypto_bn_normalize_len(exp,  (uint32_t)(exp_arr->size  / sizeof(uint32_t)));
    uint32_t nlen  = crypto_bn_normalize_len(n,    (uint32_t)(n_arr->size    / sizeof(uint32_t)));

    lbm_value result, tmp_mul, scratch_q, scratch_an, scratch_bn;
    // TODO: We can allocate and free the scratchpads immediately
    // using lbm_malloc and lbm_free so we dont have to wait for GC to reclaim
    // these memory areas.
    if (lbm_heap_allocate_array(&result,    nlen         * sizeof(uint32_t)) &&
        lbm_heap_allocate_array(&tmp_mul,   2*nlen       * sizeof(uint32_t)) &&
        lbm_heap_allocate_array(&scratch_q, 2*nlen       * sizeof(uint32_t)) &&
        lbm_heap_allocate_array(&scratch_an,(2*nlen + 1) * sizeof(uint32_t)) &&
        lbm_heap_allocate_array(&scratch_bn, nlen        * sizeof(uint32_t))) {
      crypto_bn_modexp(base, blen, exp, elen, n, nlen,
                (uint32_t*)lbm_dec_array_r(result)->data,
                (uint32_t*)lbm_dec_array_r(tmp_mul)->data,
                (uint32_t*)lbm_dec_array_r(scratch_q)->data,
                (uint32_t*)lbm_dec_array_r(scratch_an)->data,
                (uint32_t*)lbm_dec_array_r(scratch_bn)->data);
      r = result;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

// ////////////////////////////////////////////////////////////
// Init
//

void lbm_crypto_extensions_init(void) {
  lbm_add_extension("sha256-str", ext_sha256_str);
  lbm_add_extension("sha256", ext_sha256);

  // aes on blocks of 16bytes using 16 or 32 byte keys.
  lbm_add_extension("aes128-enc", ext_aes128_enc);
  lbm_add_extension("aes128-dec", ext_aes128_dec);
  lbm_add_extension("aes256-enc", ext_aes256_enc);
  lbm_add_extension("aes256-dec", ext_aes256_dec);

  lbm_add_extension("bytes-to-hex", ext_bytes_to_hex);
  lbm_add_extension("hex-to-bytes", ext_hex_to_bytes);

  // Bignum library
  lbm_add_extension("bn-add",        ext_bn_add);
  lbm_add_extension("bn-cmp",        ext_bn_cmp);
  lbm_add_extension("bn-divmod",     ext_bn_divmod);
  lbm_add_extension("bn-from-bytes", ext_bn_from_bytes);
  lbm_add_extension("bn-from-u32",   ext_bn_from_u32);
  lbm_add_extension("bn-modexp",     ext_bn_modexp);
  lbm_add_extension("bn-mul",        ext_bn_mul);
  lbm_add_extension("bn-sub",        ext_bn_sub);
  lbm_add_extension("bn-to-bytes",   ext_bn_to_bytes);
  lbm_add_extension("bn-to-u32",     ext_bn_to_u32);
}
