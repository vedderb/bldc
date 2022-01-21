/*
    Copyright 2019, 2022 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef COMPRESSION_H_
#define COMPRESSION_H_

#include <stdint.h>
#include <stdbool.h>
#include "lispbm_types.h"

typedef struct {
  uint32_t compressed_bits;
  uint32_t i;
  bool string_mode;
  char last_string_char;
  char *src;
} decomp_state; 

#define DECOMP_BUFF_SIZE 32

typedef struct {
  decomp_state ds;
  char decomp_buff[DECOMP_BUFF_SIZE];
  int  decomp_bytes;
  int  buff_pos;
} tokenizer_compressed_state_t;

extern void lbm_init_compression_state(decomp_state *s, char *src);

/* 
   Compress performs destructive changes to 
   the input string and cannot be called on constant string literal pointers 
   for example.
 
   Compress returns an array that caller must free 
*/ 
extern char *lbm_compress(char *string, uint32_t *res_size);
extern int  lbm_decompress_incremental(decomp_state *s, char *dest_buff, uint32_t dest_n);
extern bool lbm_decompress(char *dest, uint32_t dest_n, char *src);

/* parse compressed code */
extern void lbm_create_char_stream_from_compressed(tokenizer_compressed_state_t *ts,
                                                   lbm_tokenizer_char_stream_t *str,
                                                   char *bytes);

#endif
