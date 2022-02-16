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

typedef uint32_t VALUE; // A Lisp value.
typedef uint32_t TYPE;  // Representation of a type.

typedef uint32_t UINT;
typedef int32_t  INT;
typedef float    FLOAT;

#define PRI_VALUE PRIu32
#define PRI_TYPE  PRIu32
#define PRI_UINT  PRIu32
#define PRI_INT   PRId32
#define PRI_FLOAT "f"

typedef uint16_t CID;
#define CID_MAX   65535

/* tokenizer */

typedef struct tcs{
  void *state;
  bool (*more)(struct tcs);
  char (*get)(struct tcs);
  char (*peek)(struct tcs, unsigned int);
  void (*drop)(struct tcs, unsigned int);
} tokenizer_char_stream;



#endif
