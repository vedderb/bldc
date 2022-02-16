/*
    Copyright 2019, 2021 Joel Svensson  svenssonjoel@yahoo.se

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

#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#include "compression.h"
#include "lispbm_types.h"
#include "tokpar.h"

#define  KEY  0
#define  CODE 1

/* The codes are generated using python script in utils directory
   - Depends on the Huffman library (pip3 install huffman)
   - exec(open('gen_codes.py').read())
   - print(make_c())
*/

#define NUM_CODES 69
#define MAX_KEY_LENGTH 6
#define MAX_CODE_LENGTH 10
char *codes[NUM_CODES][2] = {
    { "nil", "0011001110" },
    { "cdr", "0011001111" },
    { "car", "001100000" },
    { "cons", "001100001" },
    { "let", "001100010" },
    { "define", "001100011" },
    { "progn", "001100100" },
    { "quote", "0011001010" },
    { "list", "0011001011" },
    { "if", "0011001100" },
    { "lambda", "0011001101" },
    { "((", "1101" },
    { "))", "1110" },
    { ")", "000" },
    { "(", "1111" },
    { "?", "101010" },
    { "z", "011000" },
    { "y", "101100" },
    { "x", "101011" },
    { "w", "101001" },
    { "v", "100000" },
    { "u", "011111" },
    { "t", "110001" },
    { "s", "010110" },
    { "r", "110011" },
    { "q", "100100" },
    { "p", "100001" },
    { "o", "110000" },
    { "n", "101111" },
    { "m", "00100" },
    { "l", "101110" },
    { "k", "00101" },
    { "j", "100010" },
    { "i", "011010" },
    { "h", "100110" },
    { "g", "010111" },
    { "f", "011011" },
    { "e", "100101" },
    { "d", "101101" },
    { "c", "011110" },
    { "b", "101000" },
    { "a", "011001" },
    { "9", "1000110" },
    { "8", "1000111" },
    { "7", "1100100" },
    { "6", "1100101" },
    { "5", "1001110" },
    { "4", "1001111" },
    { "3", "0111010" },
    { "2", "0111011" },
    { "1", "0111000" },
    { "0", "0111001" },
    { "_", "0101001" },
    { ",@", "0101000" },
    { ",", "0011010" },
    { "`", "0100011" },
    { " ", "0011100" },
    { "'", "0100100" },
    { "\\", "0100101" },
    { "\"", "0011111" },
    { "#", "0100111" },
    { ".", "0100110" },
    { ">", "0011011" },
    { "<", "0011101" },
    { "=", "0011110" },
    { "/", "0101011" },
    { "*", "0100000" },
    { "-", "0101010" },
    { "+", "0100010" }
    };


int match_longest_key(char *string) {

  int longest_match_ix = -1;
  unsigned int longest_match_length = 0;
  unsigned int n = strlen(string);

  for (int i = 0; i < NUM_CODES; i ++) {
    unsigned int s_len = strlen(codes[i][KEY]);
    if (s_len <= n) {
      if (strncmp(codes[i][KEY], string, s_len) == 0) {
        if (s_len > longest_match_length) {
          longest_match_ix = i;
          longest_match_length = s_len;
        }
      }
    }
  }
  return longest_match_ix;
}

int match_longest_code(char *string, uint32_t start_bit, uint32_t total_bits) {

  uint32_t bits_left = total_bits - start_bit;
  int longest_match_ix = -1;
  unsigned int longest_match_length = 0;

  for (int i = 0; i < NUM_CODES; i++) {
    unsigned int s_len = strlen(codes[i][CODE]);
    if ((uint32_t)s_len <= bits_left) {
      bool match = true;
      for (uint32_t b = 0; b < (uint32_t)s_len; b ++) {
        uint32_t byte_ix = (start_bit + b) / 8;
        uint32_t bit_ix  = (start_bit + b) % 8;

        char *code_str = codes[i][CODE];

        if (((string[byte_ix] & (1 << bit_ix)) ? '1' : '0') !=
              code_str[b]) {
          match = false;
        }
      }
      if (match && (s_len > longest_match_length)) {
        longest_match_length = s_len;
        longest_match_ix = i;
      }
    }
  }
  return longest_match_ix;
}

int compressed_length(char *string) {
  uint32_t i = 0;

  uint32_t n = strlen(string);
  int comp_len = 0; // in bits

  bool string_mode = false;
  bool gobbling_whitespace = false;

  while (i < n) {
    if (string_mode) {
      if (string[i] == '\"'  &&
          !(string[i-1] == '\\')) {
        string_mode = false;
        comp_len += 8;
        i++;
      } else {
        comp_len += 8;
        i++;
      }

    } else {

      // Gobble up any comments
      if (string[i] == ';' ) {
        while (string[i] && string[i] != '\n') {
          i++;
        }
        continue;
      }

      if ( string[i] == '\n' ||
           string[i] == ' '  ||
           string[i] == '\t' ||
           string[i] == '\r') {
        gobbling_whitespace = true;
        i ++;
        continue;
      } else if (gobbling_whitespace) {
        gobbling_whitespace = false;
        i--;
      }

      if (string[i] == '\"') string_mode = true;

      int ix;
      if (isspace(string[i])) {
        ix = match_longest_key(" ");
      } else {
        ix = match_longest_key(string + i);
      }

      if (ix == -1)return -1;
      unsigned int code_len = strlen(codes[ix][1]);
      comp_len += (int)code_len;
      i += strlen(codes[ix][0]);
    }
  }
  return comp_len;
}

void set_bit(char *c, int bit_pos, bool set) {
  char bval = 0;
  if (bit_pos <= 7) {
    bval = (char)(1 << bit_pos);
  }
  if (set) {
    *c = (char)(*c | bval);
  } else {
    *c = (char)(*c & ~bval);
  }
}

void emit_string_char_code(char *compressed, char c, int *bit_pos) {

  for (int i = 0; i < 8; i ++) {
    int byte_ix = (*bit_pos) / 8;
    int bit_ix  = (*bit_pos) % 8;
    bool s = (c & (1 << i));
    set_bit(&compressed[byte_ix], bit_ix, s);
    *bit_pos = *bit_pos + 1;
  }
}

void emit_code(char *compressed, char *code, int *bit_pos) {
  unsigned int n = strlen(code);

  for (unsigned int i = 0; i < n; i ++) {
    int byte_ix = (*bit_pos) / 8;
    int bit_ix  = (*bit_pos) % 8;
    bool s = (code[i] == '1');
    set_bit(&compressed[byte_ix], bit_ix, s);
    *bit_pos = *bit_pos + 1;
  }
}

void emit_key(char *dest, char *key, int nk, uint32_t *char_pos) {

  for (int i = 0; i < nk; i ++) {
    dest[*char_pos] = key[i];
    *char_pos = *char_pos + 1;
  }
}

char read_character(char *src, uint32_t *bit_pos) {

  char c = 0;

  for (int i = 0; i < 8; i ++) {
    uint32_t byte_ix = (*bit_pos)/8;
    uint32_t bit_ix  = (*bit_pos)%8;
    bool s = src[byte_ix] & (1 << bit_ix);
    set_bit(&c, i, s);
    *bit_pos = *bit_pos + 1;
  }
  return c;
}

char *compression_compress(char *string, uint32_t *res_size) {

  int c_size_bits_i = compressed_length(string);
  uint32_t c_size_bits = 0;
  if ( c_size_bits_i >= 0 ) {
    c_size_bits = (uint32_t)compressed_length(string);
  }

  uint32_t c_size_bytes = 4 + (c_size_bits/8);
  if (c_size_bits % 8 > 0) {
    c_size_bytes += 1;
  }
  
  uint32_t header_value = c_size_bits;

  if (header_value == 0) return NULL;

  char *compressed = malloc(c_size_bytes);
  if (!compressed) return NULL;
  memset(compressed, 0, c_size_bytes);
  *res_size = c_size_bytes;
  int bit_pos = 0;

  compressed[0] = (char)header_value;
  compressed[1] = (char)(header_value >> 8);
  compressed[2] = (char)(header_value >> 16);
  compressed[3] = (char)(header_value >> 24);
  bit_pos = 32;

  bool string_mode = false;
  bool gobbling_whitespace = false;
  uint32_t n = strlen(string);
  uint32_t i = 0;

  while (i < n) {
    if (string_mode) {

      if (string[i] == '\"' &&
          !(string[i-1] == '\\')) {
        emit_string_char_code(compressed, '\"', &bit_pos);
        i ++;
        string_mode = false;
        continue;
      } else {
        emit_string_char_code(compressed, string[i], &bit_pos);
        i++;
      }

    } else {

      // Gobble up any comments
      if (string[i] == ';' ) {
        while (string[i] && string[i] != '\n') {
          i++;
        }
        continue;
      }

      // gobble up whitespaces
      if ( string[i] == '\n' ||
           string[i] == ' '  ||
           string[i] == '\t' ||
           string[i] == '\r') {
        gobbling_whitespace = true;
        *(string + i) = ' ';
        i ++;
        continue;
      } else if (gobbling_whitespace) {
        gobbling_whitespace = false;
        i--;
      }

      /* Compress string-starting " character */
      if (string[i] == '\"') {
        string_mode = true;
      }
      int ix = match_longest_key(&string[i]);

      if (ix == -1) {
        free(compressed);
        return NULL;
      }

      emit_code(compressed, codes[ix][CODE], &bit_pos);

      i += strlen(codes[ix][0]);
    }
  }

  return compressed;
}


void compression_init_state(decomp_state *s, char *src) {
  memcpy(&s->compressed_bits, src, 4);
  s->i = 32;
  s->string_mode = false;
  s->last_string_char = 0;
  s->src = src;
}

int compression_decompress_incremental(decomp_state *s, char *dest_buff, uint32_t dest_n) {

  memset(dest_buff, 0, dest_n);
  uint32_t char_pos = 0;

  if (s->i < s->compressed_bits + 32) {
     if (s->string_mode) {
      char c = read_character(s->src, &s->i);
      if (c == '\"') {
        if (s->last_string_char != '\\') {
          s->string_mode = false;
          s->last_string_char = 0;
        }
      }
      s->last_string_char = c;
      dest_buff[0] = c;
      return 1;
    }

    int ix = match_longest_code(s->src, s->i, (s->compressed_bits + 32));
    if (ix == -1) {
      return -1;
    }

    if( strlen(codes[ix][KEY]) == 1 &&
        strncmp(codes[ix][KEY], "\"", 1) == 0) {
      s->string_mode = true;
      s->last_string_char = 0;
    }

    unsigned int n_bits_decoded = strlen(codes[ix][CODE]);
    emit_key(dest_buff, codes[ix][KEY], (int)strlen(codes[ix][KEY]), &char_pos);
    s->i+=n_bits_decoded;
    return (int)char_pos;

  } else {
    return 0;
  }

}

bool compression_decompress(char *dest, uint32_t dest_n, char *src) {

  uint32_t char_pos = 0;

  char dest_buff[32];
  int num_chars = 0;
  decomp_state s;

  memset(dest, 0, dest_n);

  compression_init_state(&s, src);

  while (true) {

    num_chars = compression_decompress_incremental(&s, dest_buff, 32);
    if (num_chars == 0) break;
    if (num_chars == -1) return false;

    for (int i = 0; i < num_chars; i ++) {
      dest[char_pos++] = dest_buff[i];
    }
  }
  return true;
}

/* Implementation of the parsing interface */

#define DECOMP_BUFF_SIZE 32
typedef struct {
  decomp_state ds;
  char decomp_buff[DECOMP_BUFF_SIZE];
  int  decomp_bytes;
  int  buff_pos;
} tokenizer_compressed_state;

bool more_compressed(tokenizer_char_stream str) {
  tokenizer_compressed_state *s = (tokenizer_compressed_state*)str.state;
  bool more =
    (s->ds.i < s->ds.compressed_bits + 32) ||
    (s->buff_pos < s->decomp_bytes);
  return more;
}

char get_compressed(tokenizer_char_stream str) {
  tokenizer_compressed_state *s = (tokenizer_compressed_state*)str.state;

  if (s->ds.i >= s->ds.compressed_bits + 32 &&
      (s->buff_pos >= s->decomp_bytes)) {
    return 0;
  }

  if (s->buff_pos >= s->decomp_bytes) {
    int n = compression_decompress_incremental(&s->ds, s->decomp_buff,DECOMP_BUFF_SIZE);
    if (n == 0) {
      return 0;
    }
    s->decomp_bytes = n;
    s->buff_pos = 0;
  }
  char c = s->decomp_buff[s->buff_pos];
  s->buff_pos += 1;
  return c;
}

char peek_compressed(tokenizer_char_stream str, unsigned int n) {
  tokenizer_compressed_state *s = (tokenizer_compressed_state*)str.state;

  tokenizer_compressed_state old;

  memcpy(&old, s, sizeof(tokenizer_compressed_state));

  char c = get_compressed(str);;
  for (unsigned int i = 1; i <= n; i ++) {
    c = get_compressed(str);
  }

  memcpy(str.state, &old, sizeof(tokenizer_compressed_state));
  return c;
}


void drop_compressed(tokenizer_char_stream str, unsigned int n) {
  for (unsigned int i = 0; i < n; i ++) {
    get_compressed(str);
  }
}


VALUE compression_parse(char *bytes) {

  tokenizer_compressed_state ts;

  ts.decomp_bytes = 0;
  memset(ts.decomp_buff, 0, 32);
  ts.buff_pos = 0;

  compression_init_state(&ts.ds, bytes);

  tokenizer_char_stream str;
  str.state = &ts;
  str.more = more_compressed;
  str.get = get_compressed;
  str.peek = peek_compressed;
  str.drop = drop_compressed;

  return tokpar_parse_program(str);
}
