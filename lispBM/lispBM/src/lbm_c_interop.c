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

lbm_cid eval_cps_load_and_eval(lbm_char_channel_t *tokenizer, bool program, bool incremental, char *name) {

  lbm_value stream;

  if (!lift_char_channel(tokenizer, &stream)) {
    return 0;
  }

  if (lbm_type_of(stream) == LBM_TYPE_SYMBOL) {
    // TODO: Check what should be done.
    return 0;
  }

  lbm_value read_mode = ENC_SYM_READ;
  if (program) {
    if (incremental) {
      read_mode = ENC_SYM_READ_AND_EVAL_PROGRAM;
    } else {
      read_mode = ENC_SYM_READ_PROGRAM;
    }
  }
  /*
   read-eval-program finishes with the result of the final expression in
   the program. This should not be passed to eval-program as it is most likely
   not a program. Even if it is a program, its not one we want to evaluate.
  */

  /* LISP ZONE */
  lbm_value launcher = lbm_cons(stream, ENC_SYM_NIL);
  launcher = lbm_cons(read_mode, launcher);
  lbm_value evaluator;
  lbm_value start_prg;
  if (read_mode == ENC_SYM_READ) {
    evaluator = lbm_cons(launcher, ENC_SYM_NIL);
    evaluator = lbm_cons(ENC_SYM_EVAL, evaluator);
    start_prg = lbm_cons(evaluator, ENC_SYM_NIL);
  } else if (read_mode == ENC_SYM_READ_PROGRAM) {
    evaluator = lbm_cons(launcher, ENC_SYM_NIL);
    evaluator = lbm_cons(ENC_SYM_EVAL_PROGRAM, evaluator);
    start_prg = lbm_cons(evaluator, ENC_SYM_NIL);
  } else { // ENC_SYM_READ_AND_EVAL_PROGRAM
    evaluator = launcher; // dummy so check below passes
    start_prg = lbm_cons(launcher, ENC_SYM_NIL);
  }

  /* LISP ZONE ENDS */

  if (lbm_type_of(launcher) != LBM_TYPE_CONS ||
      lbm_type_of(evaluator) != LBM_TYPE_CONS ||
      lbm_type_of(start_prg) != LBM_TYPE_CONS ) {
    //lbm_explicit_free_token_stream(stream);
    return 0;
  }
  return lbm_create_ctx(start_prg, ENC_SYM_NIL, 256, name);
}

lbm_cid eval_cps_load_and_define(lbm_char_channel_t *tokenizer, char *symbol, bool program) {

  lbm_value stream;

  if (!lift_char_channel(tokenizer, &stream)) {
    return 0;
  }

  if (lbm_type_of(stream) == LBM_TYPE_SYMBOL) {
    return 0;
  }

  lbm_uint sym_id;

  if (!lbm_get_symbol_by_name(symbol, &sym_id)) {
    if (!lbm_add_symbol(symbol, &sym_id)) {
      //lbm_explicit_free_token_stream(stream);
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

  if (lbm_type_of(launcher) != LBM_TYPE_CONS ||
      lbm_type_of(binding) != LBM_TYPE_CONS ||
      lbm_type_of(definer) != LBM_TYPE_CONS ) {
    //lbm_explicit_free_token_stream(stream);
    return 0;
  }
  return lbm_create_ctx(definer, lbm_enc_sym(SYM_NIL), 256, NULL);
}

lbm_cid lbm_eval_defined(char *symbol, bool program) {

  lbm_uint sym_id;

  if(!lbm_get_symbol_by_name(symbol, &sym_id)) {
    // The symbol does not exist, so it cannot be defined
    return 0;
  }

  lbm_value binding;

  if (!lbm_global_env_lookup(&binding, lbm_enc_sym(sym_id))) {
    return 0;
  }

  /* LISP ZONE */

  lbm_value launcher = lbm_cons(lbm_enc_sym(sym_id), lbm_enc_sym(SYM_NIL));
  lbm_value evaluator = launcher;
  evaluator = lbm_cons(lbm_enc_sym(program ? SYM_EVAL_PROGRAM : SYM_EVAL), evaluator);
  lbm_value start_prg = lbm_cons(evaluator, lbm_enc_sym(SYM_NIL));

  /* LISP ZONE ENDS */

  if (lbm_type_of(launcher) != LBM_TYPE_CONS ||
      lbm_type_of(evaluator) != LBM_TYPE_CONS ||
      lbm_type_of(start_prg) != LBM_TYPE_CONS ) {
    return 0;
  }
  return lbm_create_ctx(start_prg, lbm_enc_sym(SYM_NIL), 256, NULL);
}



lbm_cid lbm_load_and_eval_expression(lbm_char_channel_t *tokenizer) {
  return eval_cps_load_and_eval(tokenizer, false,false, NULL);
}

lbm_cid lbm_load_and_define_expression(lbm_char_channel_t *tokenizer, char *symbol) {
  return eval_cps_load_and_define(tokenizer, symbol, false);
}

lbm_cid lbm_load_and_eval_program(lbm_char_channel_t *tokenizer, char *name) {
  return eval_cps_load_and_eval(tokenizer, true, false, name);
}

lbm_cid lbm_load_and_eval_program_incremental(lbm_char_channel_t *tokenizer, char *name) {
  return eval_cps_load_and_eval(tokenizer, true, true, name);
}

lbm_cid lbm_load_and_define_program(lbm_char_channel_t *tokenizer, char *symbol) {
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

    if (lbm_type_of(v) == LBM_TYPE_SYMBOL &&
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
    lbm_uint ix_key = sym_id & GLOBAL_ENV_MASK;
    lbm_value *glob_env = lbm_get_global_env();
    glob_env[ix_key] = lbm_env_set(glob_env[ix_key], lbm_enc_sym(sym_id), value);
  }
  return res;
}

int lbm_undefine(char *symbol) {
  lbm_uint sym_id;
  if (!lbm_get_symbol_by_name(symbol, &sym_id))
    return 0;

  lbm_value *glob_env = lbm_get_global_env();
  lbm_uint ix_key = sym_id & GLOBAL_ENV_MASK;
  lbm_value new_env = lbm_env_drop_binding(glob_env[ix_key], lbm_enc_sym(sym_id));

  if (new_env == ENC_SYM_NOT_FOUND) return 0;
  glob_env[ix_key] = new_env;
  return 1;
}

int lbm_share_array(lbm_value *value, char *data, lbm_uint num_elt) {
  return lbm_lift_array(value, data, num_elt);
}

static bool share_const_array(lbm_value flash_cell, char *data, lbm_uint num_elt) {
  lbm_array_header_t flash_array_header;
  flash_array_header.size = num_elt;
  flash_array_header.data = (lbm_uint*)data;
  lbm_uint flash_array_header_ptr;
  lbm_flash_status s = lbm_write_const_raw((lbm_uint*)&flash_array_header,
                                           sizeof(lbm_array_header_t),
                                           &flash_array_header_ptr);
  if (s != LBM_FLASH_WRITE_OK) return false;
  s = write_const_car(flash_cell, flash_array_header_ptr);
  if (s != LBM_FLASH_WRITE_OK) return false;
  s = write_const_cdr(flash_cell, ENC_SYM_ARRAY_TYPE);
  if (s != LBM_FLASH_WRITE_OK) return false;
  return true;
}

int lbm_share_const_array(lbm_value *res, char *flash_ptr, lbm_uint num_elt) {
  lbm_value arr = 0;
  arr = LBM_PTR_BIT | LBM_TYPE_ARRAY;

  lbm_value flash_arr = 0;
  lbm_flash_status r = request_flash_storage_cell(arr, &flash_arr);
  if (r == LBM_FLASH_WRITE_OK) {
    if (!share_const_array(flash_arr, flash_ptr, num_elt)) {
      return 0;
    }
  }

  if (r == LBM_FLASH_WRITE_OK) {
    *res = flash_arr;
    return 1;
  }
  return 0;
}

int lbm_create_array(lbm_value *value, lbm_uint num_elt) {
  return lbm_heap_allocate_array(value, num_elt);
}
