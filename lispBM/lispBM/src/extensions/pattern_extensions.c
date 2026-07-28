/*
    Copyright 2026 Joel Svensson        svenssonjoel@yahoo.se

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

#include "extensions.h"
#include "heap.h"
#include "lbm_c_interop.h"
#include "extensions/pattern_extensions.h"
#include "luamatch.h"

#include <string.h>

static lbm_value lm_error_to_lbm(lm_error_t err) {
  static const char *msgs[] = {
    "no error",
    "pattern ends with '%'",
    "missing ']' in pattern",
    "missing arguments to '%b'",
    "missing '[' after '%f'",
    "too many captures in pattern",
    "invalid pattern capture (unmatched ')')",
    "invalid or unfinished capture index",
    "pattern nested too deeply",
    "pattern matching exceeded the iteration limit",
  };
  size_t idx = (size_t)err;
  if (idx >= sizeof(msgs) / sizeof(msgs[0])) idx = 0;
  lbm_set_error_reason((char*)msgs[idx]);
  return ENC_SYM_EERROR;
}

static lbm_value materialize_one(lm_match_state_t *ms, int i, const char *text,
                                  size_t whole_start, size_t whole_end) {
  if (ms->level == 0) {
    return span_to_lbm(text + whole_start, whole_end - whole_start);
  }
  lm_capture_t c = ms->capture[i];
  if (c.len == LM_CAP_POSITION) {
    return lbm_enc_i((lbm_int)(c.init - ms->src_init));
  }
  return span_to_lbm(c.init, (size_t)c.len);
}

static lbm_value materialize_result(lm_match_state_t *ms, const char *text,
                                     size_t whole_start, size_t whole_end) {
  if (ms->level == 0) {
    return materialize_one(ms, 0, text, whole_start, whole_end);
  }
  lbm_value res = ENC_SYM_NIL;
  for (int i = ms->level - 1; i >= 0; i--) {
    lbm_value v = materialize_one(ms, i, text, whole_start, whole_end);
    if (v == ENC_SYM_MERROR) return v;
    res = lbm_cons(v, res);
    if (res == ENC_SYM_MERROR) return res;
  }
  return res;
}

/* shared arg decoding for (text pattern [start-offset] [max-iterations]) */
static bool decode_find_args(lbm_value *args, lbm_uint argn,
                              char **text, size_t *text_len,
                              char **pat, size_t *pat_len,
                              size_t *start_off, size_t *max_iter) {
  char *t, *p; size_t ts, ps;
  if (!lbm_dec_str_size(args[0], &t, &ts)) return false;
  if (!lbm_dec_str_size(args[1], &p, &ps)) return false;
  *text = t; *pat = p;
  *text_len = strlen_max(t, ts);
  *pat_len = strlen_max(p, ps);
  if (!dec_opt_uint(args, argn, 2, 0, start_off)) return false;
  if (!dec_opt_uint(args, argn, 3, 0, max_iter)) return false;
  return true;
}

static lbm_value ext_str_lua_find(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || argn > 4) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }
  char *text, *pat; size_t text_len, pat_len, start_off, max_iter;
  if (!decode_find_args(args, argn, &text, &text_len, &pat, &pat_len,
                         &start_off, &max_iter)) {
    return ENC_SYM_TERROR;
  }

  lm_match_state_t ms;
  size_t s, e;
  lm_status_t st = lm_find(text, text_len, pat, pat_len, start_off, max_iter,
                            &ms, &s, &e);
  if (st == LM_ERROR) return lm_error_to_lbm(ms.error);
  if (st == LM_NOMATCH) return ENC_SYM_NIL;

  lbm_value result = materialize_result(&ms, text, s, e);
  if (result == ENC_SYM_MERROR) return result;
  lbm_value lst = lbm_cons(result, ENC_SYM_NIL);
  if (lst == ENC_SYM_MERROR) return lst;
  lst = lbm_cons(lbm_enc_i((lbm_int)e), lst);
  if (lst == ENC_SYM_MERROR) return lst;
  lst = lbm_cons(lbm_enc_i((lbm_int)s), lst);
  return lst;
}

static lbm_value ext_str_lua_match(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || argn > 4) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }
  char *text, *pat; size_t text_len, pat_len, start_off, max_iter;
  if (!decode_find_args(args, argn, &text, &text_len, &pat, &pat_len,
                         &start_off, &max_iter)) {
    return ENC_SYM_TERROR;
  }

  lm_match_state_t ms;
  size_t s, e;
  lm_status_t st = lm_find(text, text_len, pat, pat_len, start_off, max_iter,
                            &ms, &s, &e);
  if (st == LM_ERROR) return lm_error_to_lbm(ms.error);
  if (st == LM_NOMATCH) return ENC_SYM_NIL;
  return materialize_result(&ms, text, s, e);
}

/* (str-lua-gmatch text pattern state [max-iterations])
   state: nil, or a single integer (the previous match's end) from a
   prior call's next-state. */
static lbm_value ext_str_lua_gmatch(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || argn > 4) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }
  char *text, *pat; size_t text_size, pat_size;
  if (!lbm_dec_str_size(args[0], &text, &text_size)) return ENC_SYM_TERROR;
  if (!lbm_dec_str_size(args[1], &pat, &pat_size)) return ENC_SYM_TERROR;
  size_t text_len = strlen_max(text, text_size);
  size_t pat_len = strlen_max(pat, pat_size);

  long search_from = 0;
  long lastmatch = -1;
  lbm_value state = args[2];
  if (!lbm_is_symbol_nil(state)) {
    if (!lbm_is_number(state)) return ENC_SYM_TERROR;
    search_from = lastmatch = (long)lbm_dec_as_i32(state);
  }

  size_t max_iter;
  if (!dec_opt_uint(args, argn, 3, 0, &max_iter)) return ENC_SYM_TERROR;

  for (;;) {
    if (search_from < 0 || (size_t)search_from > text_len) return ENC_SYM_NIL;
    lm_match_state_t ms;
    size_t s, e;
    lm_status_t st = lm_find(text, text_len, pat, pat_len,
                              (size_t)search_from, max_iter, &ms, &s, &e);
    if (st == LM_ERROR) return lm_error_to_lbm(ms.error);
    if (st == LM_NOMATCH) return ENC_SYM_NIL;
    if ((long)e == lastmatch) {
      /* empty match at the same spot as last time: reject, advance past
         it and retry -- mirrors Lua's gmatch_aux `e != lastmatch` check.
         Always terminates: search_from strictly increases and is capped
         by text_len above. */
      search_from = (long)s + 1;
      continue;
    }
    lbm_value result = materialize_result(&ms, text, s, e);
    if (result == ENC_SYM_MERROR) return result;
    lbm_value next_state = lbm_enc_i((lbm_int)e);
    return lbm_cons(result, next_state);
  }
}

void lbm_pattern_extensions_init(void) {
  lbm_add_extension("str-lua-find", ext_str_lua_find);
  lbm_add_extension("str-lua-match", ext_str_lua_match);
  lbm_add_extension("str-lua-gmatch", ext_str_lua_gmatch);
}
