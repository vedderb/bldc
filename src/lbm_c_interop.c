/*
    Copyright 2022 Joel Svensson    svenssonjoel@yahoo.se

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

#include "lbm_c_interop.h"



/****************************************************/
/* Interface for loading and running programs and   */
/* expressions                                      */

lbm_cid eval_cps_load_and_eval(lbm_tokenizer_char_stream_t *tokenizer, bool program) {

  lbm_value stream = lbm_create_token_stream(tokenizer);

  if (lbm_type_of(stream) == LBM_VAL_TYPE_SYMBOL) {
    // TODO: Check what should be done.
    return 0;
  }

  /* LISP ZONE */

  lbm_value launcher = lbm_cons(stream, lbm_enc_sym(SYM_NIL));
  launcher = lbm_cons(lbm_enc_sym(program ? SYM_READ_PROGRAM : SYM_READ), launcher);
  lbm_value evaluator = lbm_cons(launcher, lbm_enc_sym(SYM_NIL));
  evaluator = lbm_cons(lbm_enc_sym(program ? SYM_EVAL_PROGRAM : SYM_EVAL), evaluator);
  lbm_value start_prg = lbm_cons(evaluator, lbm_enc_sym(SYM_NIL));

  /* LISP ZONE ENDS */

  if (lbm_type_of(launcher) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(evaluator) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(start_prg) != LBM_PTR_TYPE_CONS ) {
    lbm_memory_free((uint32_t*)stream);
    return 0;
  }
  return lbm_create_ctx(start_prg, lbm_enc_sym(SYM_NIL), 256);
}

lbm_cid eval_cps_load_and_define(lbm_tokenizer_char_stream_t *tokenizer, char *symbol, bool program) {

  lbm_value stream = lbm_create_token_stream(tokenizer);

  if (lbm_type_of(stream) == LBM_VAL_TYPE_SYMBOL) {
    return 0;
  }

  lbm_uint sym_id;

  if (!lbm_get_symbol_by_name(symbol, &sym_id)) {
    if (!lbm_add_symbol(symbol, &sym_id)) {
      lbm_memory_free((uint32_t*)stream);
      return 0;
    }
  }

  /* LISP ZONE */

  lbm_value launcher = lbm_cons(stream, lbm_enc_sym(SYM_NIL));
  launcher = lbm_cons(lbm_enc_sym(program ? SYM_READ_PROGRAM : SYM_READ), launcher);
  lbm_value binding = lbm_cons(launcher, lbm_enc_sym(SYM_NIL));
  binding = lbm_cons(lbm_enc_sym(sym_id), binding);
  lbm_value definer = lbm_cons(lbm_enc_sym(SYM_DEFINE), binding);
  definer  = lbm_cons(definer, lbm_enc_sym(SYM_NIL));
  /* LISP ZONE ENDS */

  if (lbm_type_of(launcher) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(binding) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(definer) != LBM_PTR_TYPE_CONS ) {
    lbm_memory_free((uint32_t*)stream);
    return 0;
  }
  return lbm_create_ctx(definer, lbm_enc_sym(SYM_NIL), 256);
}

lbm_cid lbm_eval_defined(char *symbol, bool program) {

  lbm_uint sym_id;

  if(!lbm_get_symbol_by_name(symbol, &sym_id)) {
    // The symbol does not exist, so it cannot be defined
    return 0;
  }

  lbm_value binding = lbm_env_lookup(lbm_enc_sym(sym_id), *lbm_get_env_ptr());

  if (lbm_type_of(binding) == LBM_VAL_TYPE_SYMBOL &&
      lbm_dec_sym(binding) == SYM_NOT_FOUND) {
    return 0;
  }

  /* LISP ZONE */

  lbm_value launcher = lbm_cons(lbm_enc_sym(sym_id), lbm_enc_sym(SYM_NIL));
  lbm_value evaluator = launcher;
  evaluator = lbm_cons(lbm_enc_sym(program ? SYM_EVAL_PROGRAM : SYM_EVAL), evaluator);
  lbm_value start_prg = lbm_cons(evaluator, lbm_enc_sym(SYM_NIL));

  /* LISP ZONE ENDS */

  if (lbm_type_of(launcher) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(evaluator) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(start_prg) != LBM_PTR_TYPE_CONS ) {
    return 0;
  }
  return lbm_create_ctx(start_prg, lbm_enc_sym(SYM_NIL), 256);
}



lbm_cid lbm_load_and_eval_expression(lbm_tokenizer_char_stream_t *tokenizer) {
  return eval_cps_load_and_eval(tokenizer, false);
}

lbm_cid lbm_load_and_define_expression(lbm_tokenizer_char_stream_t *tokenizer, char *symbol) {
  return eval_cps_load_and_define(tokenizer, symbol, false);
}

lbm_cid lbm_load_and_eval_program(lbm_tokenizer_char_stream_t *tokenizer) {
  return eval_cps_load_and_eval(tokenizer, true);
}

lbm_cid lbm_load_and_define_program(lbm_tokenizer_char_stream_t *tokenizer, char *symbol) {
  return eval_cps_load_and_define(tokenizer, symbol, true);
}

lbm_cid lbm_eval_defined_expression(char *symbol) {
  return lbm_eval_defined(symbol, false);
}

lbm_cid lbm_eval_defined_program(char *symbol) {
  return lbm_eval_defined(symbol, true);
}

int lbm_send_message(lbm_cid cid, lbm_value msg) {
  int res = 0;

  if (lbm_get_eval_state() == EVAL_CPS_STATE_PAUSED) {

    lbm_value v = lbm_find_receiver_and_send(cid, msg);

    if (lbm_type_of(v) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(v) == SYM_TRUE) {
      res = 1;
    }
  }
  return res;
}

int lbm_define(char *symbol, lbm_value value) {
  int res = 0;

  lbm_uint sym_id;
  if (lbm_get_eval_state() == EVAL_CPS_STATE_PAUSED) {

    if (!lbm_get_symbol_by_name(symbol, &sym_id)) {
      if (!lbm_add_symbol_const(symbol, &sym_id)) {
        return 0;
      }
    }
    lbm_env_set(lbm_get_env(), lbm_enc_sym(sym_id), value);
  }
  return res;
}
