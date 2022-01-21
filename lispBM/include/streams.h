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

typedef struct lbm_stream_s{
  void  *state;   /* stream implementation dependent state */
  lbm_value (*more)(struct lbm_stream_s*);
  lbm_value (*get)(struct lbm_stream_s*);
  lbm_value (*peek)(struct lbm_stream_s*, lbm_value);
  lbm_value (*drop)(struct lbm_stream_s*, lbm_value);
  lbm_value (*put)(struct lbm_stream_s*, lbm_value);
} lbm_stream_t;


extern lbm_value lbm_stream_get(lbm_stream_t *str);
extern lbm_value lbm_stream_more(lbm_stream_t *str);
extern lbm_value lbm_stream_peek(lbm_stream_t *str, lbm_value n);
extern lbm_value lbm_stream_drop(lbm_stream_t *str, lbm_value n);
extern lbm_value lbm_stream_put(lbm_stream_t *str, lbm_value v);

extern lbm_value lbm_stream_create(lbm_stream_t *str);

#endif
