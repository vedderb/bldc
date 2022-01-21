/*
    Copyright 2019 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef LISPBM_TYPES_H_
#define LISPBM_TYPES_H_

#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>

typedef uint32_t lbm_value; // A Lisp value.
typedef uint32_t lbm_type;  // Representation of a type.

typedef uint32_t lbm_uint;
typedef int32_t  lbm_int;
typedef float    lbm_float;

#define PRI_VALUE PRIu32
#define PRI_TYPE  PRIu32
#define PRI_UINT  PRIu32
#define PRI_INT   PRId32
#define PRI_FLOAT "f"

typedef uint16_t lbm_cid;
#define CID_MAX   65535

/* tokenizer */

typedef struct lbm_tcs{
  void *state;
  bool (*more)(struct lbm_tcs*);
  char (*get)(struct lbm_tcs*);
  char (*peek)(struct lbm_tcs*, unsigned int);
  void (*drop)(struct lbm_tcs*, unsigned int);
} lbm_tokenizer_char_stream_t;



#endif
