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

#include "streams.h"
#include "heap.h"

VALUE stream_get(stream_t *str) {
  return str->get(str);
}

VALUE stream_more(stream_t *str) {
  return str->more(str);
}

VALUE stream_peek(stream_t *str, VALUE n) {
  return str->peek(str,n);
}

VALUE stream_drop(stream_t *str, VALUE n) {
  return str->drop(str,n);
}

VALUE stream_put(stream_t *str, VALUE v) {
  return str->put(str,v);
}

VALUE stream_create(stream_t *str) {
  VALUE s = cons((VALUE)str, enc_sym(SYM_STREAM_TYPE));
  if (type_of(s) == PTR_TYPE_CONS) {
    set_ptr_type(s, SYM_TYPE_STREAM);
  }
  return s;
}
