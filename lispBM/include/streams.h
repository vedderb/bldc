/*
    Copyright 2021 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef STREAMS_H_
#define STREAMS_H_

#include "lispbm_types.h"

typedef struct stream_s{
  void  *state;   /* stream implementation dependent state */
  VALUE (*more)(struct stream_s*);
  VALUE (*get)(struct stream_s*);
  VALUE (*peek)(struct stream_s*, VALUE);
  VALUE (*drop)(struct stream_s*, VALUE);
  VALUE (*put)(struct stream_s*, VALUE);
} stream_t;


extern VALUE stream_get(stream_t *str);
extern VALUE stream_more(stream_t *str);
extern VALUE stream_peek(stream_t *str, VALUE n);
extern VALUE stream_drop(stream_t *str, VALUE n);
extern VALUE stream_put(stream_t *str, VALUE v);

extern VALUE stream_create(stream_t *str);

#endif
