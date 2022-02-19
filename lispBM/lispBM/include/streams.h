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
/** \file streams.h
 *  lbm_stream_t implements an abstract stream that can be given many different implementations.
 *  The operations more, get, peek, drop, put should be implemented for all streams but if they are
 *  not used or do not make sense for the particular stream they can return for example enc_sym(SYM_NIL).
 *
 * */

#ifndef STREAMS_H_
#define STREAMS_H_

#include "lbm_types.h"

typedef struct lbm_stream_s{
  void  *state;   /* stream implementation dependent state */
  lbm_value (*more)(struct lbm_stream_s*);
  lbm_value (*get)(struct lbm_stream_s*);
  lbm_value (*peek)(struct lbm_stream_s*, lbm_value);
  lbm_value (*drop)(struct lbm_stream_s*, lbm_value);
  lbm_value (*put)(struct lbm_stream_s*, lbm_value);
} lbm_stream_t;

/** Get a value from a stream.
 *
 * \param str Stream to get a value from.
 * \return A value.
 */
extern lbm_value lbm_stream_get(lbm_stream_t *str);
/** Check if there is more data available on the stream.
 *
 * \param str Stream to query
 * \return A stream implementation dependent value indicating if there is more data or not.
 */
extern lbm_value lbm_stream_more(lbm_stream_t *str);
/** Read the nth value of a stream without removing it from the stream.
 *
 * \param str Stream to peek into.
 * \param n Index to peek at.
 * \return Value at that position.
 */
extern lbm_value lbm_stream_peek(lbm_stream_t *str, lbm_value n);
/** Drop n values from a stream.
 *
 * \param str Stream to drop values from.
 * \param n Number of values to drop.
 * \return A stream implementation dependent value.
 */
extern lbm_value lbm_stream_drop(lbm_stream_t *str, lbm_value n);
/** Put a value onto a stream
 *
 * \param str Stream to put a value onto.
 * \param v Value to put onto the stream.
 * \return A Stream implementation dependent value.
 */
extern lbm_value lbm_stream_put(lbm_stream_t *str, lbm_value v);

/** Create a lispbm value (lbm_value) representing a stream.
 *
 * \param str Stream to lift into the lisp.
 * \return The lbm_value that represents the stream.
 */
extern lbm_value lbm_stream_create(lbm_stream_t *str);

#endif
