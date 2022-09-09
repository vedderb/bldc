/*
    Copyright 2021, 2022 Joel Svensson        svenssonjoel@yahoo.se

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

#include "streams.h"
#include "heap.h"
#include "lbm_types.h"
#include "lbm_memory.h"
#include "symrepr.h"

lbm_value lbm_stream_get(lbm_stream_t *str) {
  return str->get(str);
}

lbm_value lbm_stream_more(lbm_stream_t *str) {
  return str->more(str);
}

lbm_value lbm_stream_peek(lbm_stream_t *str, lbm_value n) {
  return str->peek(str,n);
}

lbm_value lbm_stream_drop(lbm_stream_t *str, lbm_value n) {
  return str->drop(str,n);
}

lbm_value lbm_stream_put(lbm_stream_t *str, lbm_value v) {
  return str->put(str,v);
}

lbm_value lbm_stream_create(lbm_stream_t *str) {
  lbm_value s = lbm_cons((lbm_value)str, lbm_enc_sym(SYM_STREAM_TYPE));
  if (lbm_type_of(s) == LBM_TYPE_CONS) {
    s = lbm_set_ptr_type(s, LBM_TYPE_STREAM);
  }
  return s;
}

// Lift low-level character streams to lisp stream.

static lbm_value lift_get(lbm_stream_t *str) {
  lbm_tokenizer_char_stream_t *cs = (lbm_tokenizer_char_stream_t*)str->state;
  return lbm_enc_char(cs->get(cs));
}

static lbm_value lift_put(lbm_stream_t *str, lbm_value v) {
  char c = lbm_dec_as_char(v);
  lbm_tokenizer_char_stream_t *cs = (lbm_tokenizer_char_stream_t*)str->state;
  return cs->put(cs, c) ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value lift_more(lbm_stream_t *str) {
  lbm_tokenizer_char_stream_t *cs = (lbm_tokenizer_char_stream_t*)str->state;
  return cs->get(cs) ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value lift_peek(lbm_stream_t *str, lbm_value n) {
  if (!lbm_is_number(n)) return ENC_SYM_TERROR;
  lbm_tokenizer_char_stream_t *cs = (lbm_tokenizer_char_stream_t*)str->state;
  return lbm_enc_char(cs->peek(cs, lbm_dec_as_u32(n)));
}

static lbm_value lift_drop(lbm_stream_t *str, lbm_value n) {
  if (!lbm_is_number(n)) return ENC_SYM_TERROR;
  lbm_tokenizer_char_stream_t *cs = (lbm_tokenizer_char_stream_t*)str->state;
  cs->drop(cs, lbm_dec_as_u32(n));
  return ENC_SYM_TRUE;
}

lbm_value lbm_stream_lift(lbm_tokenizer_char_stream_t *char_stream) {
  lbm_stream_t *stream = NULL;
  stream = (lbm_stream_t *)lbm_memory_allocate(1 + sizeof(lbm_stream_t) / sizeof(lbm_uint));
  if (stream == NULL) {
    return ENC_SYM_MERROR;
  }

  stream->state = (void*)char_stream;
  stream->more  = lift_more;
  stream->get   = lift_get;
  stream->put   = lift_put;
  stream->peek  = lift_peek;
  stream->drop  = lift_drop;

  return lbm_stream_create(stream);
}
