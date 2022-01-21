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
  lbm_value s = lbm_cons((lbm_value)str, lbm_enc_sym(SYM_TYPE_STREAM));
  if (lbm_type_of(s) == LBM_PTR_TYPE_CONS) {
    s = s | LBM_PTR_TYPE_STREAM;
  }
  return s;
}
