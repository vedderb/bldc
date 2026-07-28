
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


#ifndef CRYPTO_H_
#define CRYPTO_H_

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define CRYPTO_AES128_NUM_WORDS_PER_KEY 4
#define CRYPTO_AES256_NUM_WORDS_PER_KEY 8
  
#define CRYPTO_AES128_NUM_ROUNDS 10
#define CRYPTO_AES256_NUM_ROUNDS 14
  
  void crypto_sha256(uint8_t *res, uint8_t *bytes, uint32_t n);

  // AES
  void crypto_aes_key_expansion(uint8_t *key, uint8_t *round_keys, int nk);
  void crypto_aes_cipher(uint8_t *state, uint8_t *round_keys, int nr);
  void crypto_aes_inv_cipher(uint8_t *state, uint8_t *round_keys, int nr);

  // BN
  void crypto_bn_add(uint32_t *a, uint32_t alen,
                     uint32_t *b, uint32_t blen,
                     uint32_t *res);
  void crypto_bn_sub(uint32_t *a, uint32_t alen,
                     uint32_t *b, uint32_t blen,
                     uint32_t *res);
  void crypto_bn_mul(uint32_t *a, uint32_t alen,
                     uint32_t *b, uint32_t blen,
                     uint32_t *res);
  uint32_t crypto_bn_normalize_len(uint32_t *a, uint32_t alen);
  int crypto_bn_cmp(uint32_t *a, uint32_t alen,
                    uint32_t *b, uint32_t blen);
  void crypto_bn_divmod(uint32_t *a, uint32_t alen,
                        uint32_t *b, uint32_t blen,
                        uint32_t *q, uint32_t *r,
                        uint32_t *scratch_an,
                        uint32_t *scratch_bn);
  void crypto_bn_divmod_single(uint32_t *a, uint32_t alen,
                               uint32_t  b,
                               uint32_t *q, uint32_t *r);
  void crypto_bn_mod(uint32_t *a,      uint32_t alen,
                     uint32_t *n,      uint32_t nlen,
                     uint32_t *result, uint32_t *rlen,
                     uint32_t *scratch_q,
                     uint32_t *scratch_an,
                     uint32_t *scratch_bn);
  void crypto_bn_modexp(uint32_t *base, uint32_t blen,
                        uint32_t *exp,  uint32_t elen,
                        uint32_t *n,    uint32_t nlen,
                        uint32_t *result,
                        uint32_t *tmp_mul,
                        uint32_t *scratch_q,
                        uint32_t *scratch_an,
                        uint32_t *scratch_bn);

  // Hex utilities
  // out must be at least len*2+1 bytes.
  void crypto_bytes_to_hex(uint8_t *in, uint32_t len, char *out);
  // hex_len is the number of hex characters (excluding null terminator).
  // hex_len must be even. out must be at least hex_len/2 bytes.
  // Returns 1 on success, 0 if hex_len is odd.
  int crypto_hex_to_bytes(uint8_t *hex, uint32_t hex_len, uint8_t *out);

  
#ifdef __cplusplus
}
#endif

#endif



