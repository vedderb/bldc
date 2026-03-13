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


/* ************************************************************
 * SHA256 hash
 * As explained in FIPS PUB 180-4 but with an incremental
 * padder instead of a padding preprocessing step.
 * ************************************************************/
static inline uint32_t rotr(uint32_t x, uint32_t n) {
  return (x >> n) | (x << (32 - n));
}

static inline uint32_t ch(uint32_t x, uint32_t y, uint32_t z) {
  return (x & y) ^ (~x & z);
}

static inline uint32_t maj(uint32_t x, uint32_t y, uint32_t z) {
  return (x & y) ^ (x & z) ^ (y & z);
}

static inline uint32_t sum0_256(uint32_t x) {
  return rotr(x, 2) ^ rotr(x, 13) ^ rotr(x, 22);
}

static inline uint32_t sum1_256(uint32_t x) {
  return rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25);
}

static inline uint32_t sigma0_256(uint32_t x) {
  return rotr(x, 7) ^ rotr(x, 18) ^ (x >> 3);
}

static inline uint32_t sigma1_256(uint32_t x) {
  return rotr(x, 17) ^ rotr(x, 19) ^ (x >> 10);
}

static const uint32_t k256[64] = {
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
  0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
  0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
  0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
  0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
  0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

static const uint32_t h256[8] = {
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
};


static uint32_t read_m(uint8_t *bytes, uint32_t n, uint32_t block, uint32_t word) {
  // This does the M^(i)_t accesses on the fly by assembling a
  // big endian uint32_t from elements of the array.
  //
  // If the requested block and word contains the byte after the last byte of data,
  // the 0b10000000 is inserted into the result word.
  //
  // If the requested word is between the end of the data and the nearest multiple
  // of 512-64 bits, the output contains zeroes.
  //
  // If the block and word accessed is the last or second to last
  // the appropriate part of the size is appended.

  uint64_t size_in_bits = n * 8; // size of payload in bits.

  //                                size 
  //                           1-bit
  //                       size
  uint32_t padded_len  = (((n + 1) + 8) + 63) & ~63u;
  uint32_t size_start  = padded_len - 8;
  
  uint32_t start_byte = block * 64 + word * 4;
  uint32_t end_byte   = start_byte + 4;

  uint32_t res = 0; // prepadded with zeroes. 

  for (uint32_t i = start_byte; i < end_byte; i ++) {
    res = res << 8;
    // assemble a big endian result 
    
    if (i < n) {
      res |= bytes[i];
    } else if (i == n) {
      res |= (1 << 7); // 0b10000000;
    } else if (i >= size_start && i < padded_len) {
      uint32_t byte = i - size_start;
      res |= (uint8_t)(size_in_bits >> ((7 - byte) * 8));
    }
    // else leave as zeroes.
  }
  return res;           
}

// res is a 8*4 byte array preallocated before passed to this function. 
static void incremental_sha256(uint8_t *res, uint8_t *bytes, uint32_t n) {

  uint32_t padded_len  = (((n + 1) + 8) + 63) & ~63u;
  uint32_t blocks = padded_len / 64;  // at least 1.

  uint32_t w[64];
  uint32_t hash[8];
  memcpy(hash, h256, 8 * sizeof(uint32_t));
  
  for (uint32_t i = 0; i < blocks; i ++) {

    for (uint32_t t = 0; t < 64; t ++) {
      if (t <= 15) {
        w[t] = read_m(bytes, n, i, t);
      } else {
        w[t] = sigma1_256(w[t-2]) + w[t-7] + sigma0_256(w[t-15]) + w[t-16];
      }
    }

    uint32_t a = hash[0];
    uint32_t b = hash[1];
    uint32_t c = hash[2];
    uint32_t d = hash[3];
    uint32_t e = hash[4];
    uint32_t f = hash[5];
    uint32_t g = hash[6];
    uint32_t h = hash[7];

    for (uint32_t t  = 0; t < 64; t++) {
      uint32_t t1 = h + sum1_256(e) + ch(e,f,g)+k256[t]+w[t];
      uint32_t t2 = sum0_256(a) + maj(a,b,c);
      h = g;
      g = f;
      f = e;
      e = d + t1;
      d = c;
      c = b;
      b = a;
      a = t1 + t2;
    }
    hash[0] = a + hash[0];
    hash[1] = b + hash[1];
    hash[2] = c + hash[2];
    hash[3] = d + hash[3];
    hash[4] = e + hash[4];
    hash[5] = f + hash[5];
    hash[6] = g + hash[6];
    hash[7] = h + hash[7];
  }

  for (uint32_t t = 0; t < 32; t ++) {

    uint32_t word = t / 4;
    uint32_t byte = t % 4;
    
    res[t] = (uint8_t)(hash[word] >> ((3 - byte) * 8));
  }
}


static lbm_value ext_sha256_str(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) { 
    if(lbm_create_array(&res, 32)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      char *str = lbm_dec_str(args[0]);
      uint32_t n = strlen_max(str, arr->size);
      incremental_sha256((uint8_t*)arr->data, (uint8_t*)str, n);
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
      incremental_sha256((uint8_t*)arr->data, (uint8_t*)in_arr->data, in_arr->size);
    }
  }
  return res;
}

/* ************************************************************
 * AES 128 - 256
 *
 * Block ciphers as described in FIPS 197.
 *
 * Implements the AES 128 and 256 block cipher primitives
 * that can be used to implement ECB, CBC, CTR on a higher level.
 *  ECB - Electronic Code Book
 *  CBC - Cipher Block Chaining
 *  CTR - Counter
 *  GCM - Galois/Counter Mode
 * ************************************************************/


#define AES128_NUM_WORDS_PER_KEY 4
#define AES256_NUM_WORDS_PER_KEY 8

#define AES128_NUM_ROUNDS 10
#define AES256_NUM_ROUNDS 14


/* FIPS 197 Figure 7 — AES S-box */
static const uint8_t aes_sbox[256] = {
  0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5,
  0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
  0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0,
  0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
  0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc,
  0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
  0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a,
  0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
  0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0,
  0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
  0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b,
  0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
  0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85,
  0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
  0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
  0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
  0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17,
  0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
  0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88,
  0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
  0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c,
  0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
  0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9,
  0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
  0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6,
  0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
  0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e,
  0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
  0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94,
  0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
  0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68,
  0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
};

/* FIPS 197 Figure 14 — AES inverse S-box */
static const uint8_t aes_inv_sbox[256] = {
  0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38,
  0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
  0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87,
  0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
  0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d,
  0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
  0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2,
  0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
  0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16,
  0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
  0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda,
  0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
  0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a,
  0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
  0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02,
  0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
  0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea,
  0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
  0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85,
  0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
  0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89,
  0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
  0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20,
  0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
  0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31,
  0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
  0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d,
  0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
  0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0,
  0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
  0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26,
  0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
};

/* FIPS 197 Table 5 — Round constant (Rcon) for key schedule */
static const uint8_t aes_rcon[11] = {
  0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36
};


static void sub_bytes(uint8_t *state, const uint8_t *box) {
  for (int i = 0; i < 16; i ++) {
    state[i] = box[state[i]];
  }
}

/* FIPS 197 column-major layout: element (r,c) is at index r + 4*c
   Row r has elements at indices r, r+4, r+8, r+12.
   ShiftRows shifts row r left by r positions. */
static void shift_rows(uint8_t *state) {
  uint8_t t;
  // Row 1: left shift by 1
  t = state[1]; state[1] = state[5]; state[5] = state[9]; state[9] = state[13]; state[13] = t;
  // Row 2: left shift by 2 (swap pairs)
  t = state[2]; state[2] = state[10]; state[10] = t;
  t = state[6]; state[6] = state[14]; state[14] = t;
  // Row 3: left shift by 3 = right shift by 1
  t = state[15]; state[15] = state[11]; state[11] = state[7]; state[7] = state[3]; state[3] = t;
}

static void inv_shift_rows(uint8_t *state) {
  uint8_t t;
  // Row 1: right shift by 1
  t = state[13]; state[13] = state[9]; state[9] = state[5]; state[5] = state[1]; state[1] = t;
  // Row 2: right shift by 2 (swap pairs)
  t = state[2]; state[2] = state[10]; state[10] = t;
  t = state[6]; state[6] = state[14]; state[14] = t;
  // Row 3: right shift by 3 = left shift by 1
  t = state[3]; state[3] = state[7]; state[7] = state[11]; state[11] = state[15]; state[15] = t;
}

/* Multiply by 2 in GF(2^8) with AES irreducible polynomial 0x11b */
static inline uint8_t xtime(uint8_t x) {
  return (x << 1) ^ ((x & 0x80) ? 0x1b : 0x00);
}

static uint8_t gf_mul(uint8_t x, uint8_t y) {
    uint8_t res = 0;
    while (y) {
      if (y & 1) res ^= x;
      x = xtime(x);
      y >>= 1;
    }
    return res;
  }


/* FIPS 197 §5.1.3 — MixColumns
   Column c has elements at state[4*c .. 4*c+3] (rows 0-3).
   Each column multiplied by the matrix:
   [2 3 1 1]
   [1 2 3 1]
   [1 1 2 3]
   [3 1 1 2]  in GF(2^8) */
static void mix_columns(uint8_t *state) {
  for (int c = 0; c < 4; c++) {
    uint8_t s0 = state[4*c];
    uint8_t s1 = state[4*c + 1];
    uint8_t s2 = state[4*c + 2];
    uint8_t s3 = state[4*c + 3];
    state[4*c]     = gf_mul(2,s0) ^ gf_mul(3,s1) ^ s2           ^ s3;
    state[4*c + 1] = s0           ^ gf_mul(2,s1) ^ gf_mul(3,s2) ^ s3;
    state[4*c + 2] = s0           ^ s1           ^ gf_mul(2,s2) ^ gf_mul(3,s3);
    state[4*c + 3] = gf_mul(3,s0) ^ s1           ^ s2           ^ gf_mul(2,s3);
  }
}

/* FIPS 197 §5.3.3 — InvMixColumns
   Each column multiplied by the inverse matrix:
   [14 11 13  9]
   [ 9 14 11 13]
   [13  9 14 11]
   [11 13  9 14]  in GF(2^8) */
static void inv_mix_columns(uint8_t *state) {
  for (int c = 0; c < 4; c++) {
    uint8_t s0 = state[4*c];
    uint8_t s1 = state[4*c + 1];
    uint8_t s2 = state[4*c + 2];
    uint8_t s3 = state[4*c + 3];
    state[4*c]     = gf_mul(14,s0) ^ gf_mul(11,s1) ^ gf_mul(13,s2) ^ gf_mul( 9,s3);
    state[4*c + 1] = gf_mul( 9,s0) ^ gf_mul(14,s1) ^ gf_mul(11,s2) ^ gf_mul(13,s3);
    state[4*c + 2] = gf_mul(13,s0) ^ gf_mul( 9,s1) ^ gf_mul(14,s2) ^ gf_mul(11,s3);
    state[4*c + 3] = gf_mul(11,s0) ^ gf_mul(13,s1) ^ gf_mul( 9,s2) ^ gf_mul(14,s3);
  }
}

static void rot_word(uint8_t *w) {
  uint8_t a = w[0];
  w[0] = w[1];
  w[1] = w[2];
  w[2] = w[3];
  w[3] = a;
}

static void sub_word(uint8_t *w) {
  for (int i = 0; i < 4; i++) {
    w[i] = aes_sbox[w[i]];
  }
}

static void add_round_key(uint8_t *state, uint8_t *round_key) {
  for (int i = 0; i < 16; i++) {
    state[i] ^= round_key[i];
  }
}

/* FIPS 197 5.2 — KeyExpansion
   Expands key into round keys.
   nk = 4 for AES-128 (176 bytes output)
   nk = 8 for AES-256 (240 bytes output)
   Caller must provide round_keys buffer of 4 * (nk + 7) * 4 bytes */
static void key_expansion(uint8_t *key, uint8_t *round_keys, int nk) {
  int nr = nk + 6;
  int total_words = 4 * (nr + 1);

  memcpy(round_keys, key, (size_t)nk * 4);

  for (int i = nk; i < total_words; i++) {
    uint8_t temp[4];
    memcpy(temp, round_keys + (i - 1) * 4, 4);

    if (i % nk == 0) {
      rot_word(temp);
      sub_word(temp);
      temp[0] ^= aes_rcon[i / nk];
    } else if (nk > 6 && i % nk == 4) {
      sub_word(temp);
    }

    for (int j = 0; j < 4; j++) {
      round_keys[i * 4 + j] = round_keys[(i - nk) * 4 + j] ^ temp[j];
    }
  }
}

/* FIPS 197 5.1 — Cipher (encryption)
   state: 16-byte block (modified in place)
   round_keys: expanded key schedule
   nr: number of rounds (10 for AES-128, 14 for AES-256) */
static void cipher(uint8_t *state, uint8_t *round_keys, int nr) {
  add_round_key(state, round_keys);

  for (int round = 1; round < nr; round++) {
    sub_bytes(state, aes_sbox);
    shift_rows(state);
    mix_columns(state);
    add_round_key(state, round_keys + round * 16);
  }

  sub_bytes(state, aes_sbox);
  shift_rows(state);
  add_round_key(state, round_keys + nr * 16);
}

/* FIPS 197 5.3 — InvCipher (decryption)
   state: 16-byte block (modified in place)
   round_keys: expanded key schedule
   nr: number of rounds (10 for AES-128, 14 for AES-256) */
static void inv_cipher(uint8_t *state, uint8_t *round_keys, int nr) {
  add_round_key(state, round_keys + nr * 16);

  for (int round = nr - 1; round >= 1; round--) {
    inv_shift_rows(state);
    sub_bytes(state, aes_inv_sbox);
    add_round_key(state, round_keys + round * 16);
    inv_mix_columns(state);
  }

  inv_shift_rows(state);
  sub_bytes(state, aes_inv_sbox);
  add_round_key(state, round_keys);
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
      key_expansion((uint8_t*)key->data, key_expanded, AES128_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        cipher((uint8_t*)out->data, key_expanded, AES128_NUM_ROUNDS);
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
      key_expansion((uint8_t*)key->data, key_expanded, AES128_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        inv_cipher((uint8_t*)out->data, key_expanded, AES128_NUM_ROUNDS);
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
      key_expansion((uint8_t*)key->data, key_expanded, AES256_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        cipher((uint8_t*)out->data, key_expanded, AES256_NUM_ROUNDS);
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
      key_expansion((uint8_t*)key->data, key_expanded, AES256_NUM_WORDS_PER_KEY);
      // If this failed res is MERROR.
      if (lbm_create_array(&res, 16)) {
        lbm_array_header_t *out = (lbm_array_header_t*)lbm_car(res);
        memcpy(out->data, data->data, 16);
        inv_cipher((uint8_t*)out->data, key_expanded, AES256_NUM_ROUNDS);
      }
    } else {
      res = ENC_SYM_EERROR;
    }
  }
  return res;
}

/* ************************************************************
 * Utilities 
 * ************************************************************/

static lbm_value ext_bytes_to_hex(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
    lbm_uint hex_len = in_arr->size * 2 + 1;
    if (lbm_create_array(&res, hex_len)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      uint8_t *in = (uint8_t*)in_arr->data;
      char *out = (char*)arr->data;
      for (lbm_uint i = 0; i < in_arr->size; i++) {
        snprintf(out + i * 2, 3, "%02x", in[i]);
      }
      out[hex_len - 1] = '\0';
    }
  }
  return res;
}

static lbm_value ext_hex_to_bytes(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
    res = ENC_SYM_EERROR;
    if (in_arr->size%2 == 1) { // must be a power of 2 when excluding the terminating 0
      lbm_uint bytes_len = (in_arr->size - 1) / 2;
      if (lbm_create_array(&res, bytes_len)) {
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
        uint8_t *in = (uint8_t*)in_arr->data;
        uint8_t *out = (uint8_t*)arr->data;
        for (lbm_uint j = 0; j < bytes_len; j++) {
          lbm_uint i = j * 2;
          uint8_t byte_val = 0;
          if (in[i] >= '0' && in[i] <= '9') byte_val += (uint8_t)(16 * (in[i] - '0'));
          else if (in[i] >= 'a' &&  in [i] <= 'f') byte_val += (uint8_t)(16 * (10 + in[i] - 'a'));
          else if (in[i] >= 'A' &&  in [i] <= 'F') byte_val += (uint8_t)(16 * (10 + in[i] - 'A'));
          if (in[i+1] >= '0' && in[i+1] <= '9') byte_val += (uint8_t)(in[i+1] - '0');
          else if (in[i+1] >= 'a' &&  in [i+1] <= 'f') byte_val += (uint8_t)(10 + in[i+1] - 'a');
          else if (in[i+1] >= 'A' &&  in [i+1] <= 'F') byte_val += (uint8_t)(10 + in[i+1] - 'A');
          out[j] = byte_val;
        }
      }
    }
  }
  return res;
}

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
}
