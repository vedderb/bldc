/*
    Copyright 2020 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef BYTECODE_H_
#define BYTECODE_H_

typedef struct {
  char* symbol_str;
  VALUE symbol_indirection;
} symbol_indirection_t;

typedef struct {
  unsigned int code_size;
  uint8_t *code;
  unsigned int num_indirections;
  symbol_indirection_t *indirections;
} bytecode_t;


#endif
