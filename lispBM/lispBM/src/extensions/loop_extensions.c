/*
    Copyright 2023, 2025 Joel Svensson        svenssonjoel@yahoo.se
              2022       Benjamin Vedder      benjamin@vedder.se

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

#include <extensions.h>

static lbm_uint sym_res;
static lbm_uint sym_loop;
static lbm_uint sym_break;
static lbm_uint sym_brk;
static lbm_uint sym_rst;
static lbm_uint sym_return;


static lbm_value make_list(int num, ...) {
  va_list arguments;
  va_start (arguments, num);
  lbm_value res = ENC_SYM_NIL;
  for (int i = 0; i < num; i++) {
    res = lbm_cons(va_arg(arguments, lbm_value), res);
  }
  va_end (arguments);
  return lbm_list_destructive_reverse(res);
}


static inline lbm_value mk_lam(lbm_value args, lbm_value body) {
  return make_list(3, ENC_SYM_LAMBDA, args, body);
}

static inline lbm_value mk_call_cc(lbm_value body) {
  return make_list(2, ENC_SYM_CALL_CC_UNSAFE, body);
}

static inline lbm_value mk_let(lbm_value bindings, lbm_value body) {
  return make_list(3, ENC_SYM_LET, bindings, body);
}

static inline lbm_value mk_if(lbm_value cond, lbm_value tb, lbm_value fb) {
  return make_list(4, ENC_SYM_IF, cond, tb, fb);
}

static inline lbm_value mk_inc(lbm_value v) {
  return make_list(3, ENC_SYM_ADD, v, lbm_enc_i(1));
}

static inline lbm_value mk_lt(lbm_value a, lbm_value b) {
  return make_list(3, ENC_SYM_LT, a, b);
}

static inline lbm_value mk_eq(lbm_value a, lbm_value b) {
  return make_list(3, ENC_SYM_EQ, a, b);
}

static inline lbm_value mk_car(lbm_value a) {
  return make_list(2, ENC_SYM_CAR, a);
}

static inline lbm_value mk_cdr(lbm_value a) {
  return make_list(2, ENC_SYM_CDR, a);
}

static lbm_value ext_me_loopfor(lbm_value *args, lbm_uint argn) {
  if (argn != 5) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value start = args[1];
  lbm_value cond = args[2];
  lbm_value update = args[3];
  lbm_value body = args[4];

  // (call-cc-unsafe
  //  (lambda (break)
  //    (let ((loop (lambda (it res)
  //                  (if cond (loop update body) res))))
  //      (loop start nil))))

  lbm_value enc_sym_loop = lbm_enc_sym(sym_loop); // maybe do one time at init?
  lbm_value enc_sym_break = lbm_enc_sym(sym_break);
  lbm_value enc_sym_res = lbm_enc_sym(sym_res);

  return mk_call_cc(mk_lam(make_list(1, enc_sym_break),
                           mk_let(make_list(1,
                                            make_list(2,
                                                      enc_sym_loop,
                                                      mk_lam(make_list(2, it, enc_sym_res),
                                                             mk_if(cond,
                                                                   make_list(3, enc_sym_loop, update, body),
                                                                   enc_sym_res)))),
                                  make_list(3, enc_sym_loop, start, ENC_SYM_NIL))));
}

static lbm_value ext_me_loopwhile(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    return ENC_SYM_EERROR;
  }

  lbm_value cond = args[0];
  lbm_value body = args[1];

  //(call-cc-unsafe
  // (lambda (break)
  //   (let ((loop (lambda (res)
  //                 (if cond (loop body) res))))
  //     (loop nil))))

  lbm_value enc_sym_loop = lbm_enc_sym(sym_loop); // maybe do one time at init?
  lbm_value enc_sym_break = lbm_enc_sym(sym_break);
  lbm_value enc_sym_res = lbm_enc_sym(sym_res);

  return mk_call_cc(mk_lam(make_list(1, enc_sym_break),
                           mk_let(make_list(1,
                                            make_list(2,
                                                      enc_sym_loop,
                                                      mk_lam(make_list(1, enc_sym_res),
                                                             mk_if(cond,
                                                                   make_list(2,enc_sym_loop, body),
                                                                   enc_sym_res)))),
                                  (make_list(2, enc_sym_loop, ENC_SYM_NIL)))));
}

static lbm_value ext_me_looprange(lbm_value *args, lbm_uint argn) {
  if (argn != 4) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value start = args[1];
  lbm_value end = args[2];
  lbm_value body = args[3];

  // (call-cc-unsafe
  //  (lambda (break)
  //   (let ((loop (lambda (it res)
  //                 (if (< it end)
  //                     (loop (+ it 1) body)
  //                   res))))
  //     (loop start nil))))

  lbm_value enc_sym_loop = lbm_enc_sym(sym_loop); // maybe do one time at init?
  lbm_value enc_sym_break = lbm_enc_sym(sym_break);
  lbm_value enc_sym_res = lbm_enc_sym(sym_res);

  return mk_call_cc(mk_lam(make_list(1, enc_sym_break),
                           mk_let(make_list(1,
                                            make_list(2,
                                                      enc_sym_loop,
                                                      mk_lam(make_list(2, it, enc_sym_res),
                                                             mk_if(mk_lt(it, end),
                                                                   make_list(3,
                                                                             enc_sym_loop,
                                                                             mk_inc(it),
                                                                             body),
                                                                   enc_sym_res)))),
                                  make_list(3, enc_sym_loop, start, ENC_SYM_NIL))));
}

// TODO: Something that does not work as expected with this
//       definition of loopforeach is (loopforeach e (list nil nil nil) ...).

static lbm_value ext_me_loopforeach(lbm_value *args, lbm_uint argn) {
  if (argn != 3) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value lst = args[1];
  lbm_value body = args[2];

  //(call-cc-unsafe
  // (lambda (break)
  //   (let ((loop (lambda (rst it res)
  //                 (if (eq it nil)
  //                     res
  //                   (loop (car rst) (cdr rst) body)))))
  //     (loop (car lst) (cdr lst) nil))))

  lbm_value enc_sym_loop = lbm_enc_sym(sym_loop); // maybe do one time at init?
  lbm_value enc_sym_break = lbm_enc_sym(sym_break);
  lbm_value enc_sym_res = lbm_enc_sym(sym_res);
  lbm_value enc_sym_rst = lbm_enc_sym(sym_rst);

  return mk_call_cc(mk_lam(make_list(1, enc_sym_break),
                           mk_let(make_list(1,
                                            make_list(2,
                                                      enc_sym_loop,
                                                      mk_lam(make_list(3,
                                                                       it,
                                                                       enc_sym_rst,
                                                                       enc_sym_res),
                                                             mk_if(mk_eq(it, ENC_SYM_NIL),
                                                                   enc_sym_res,
                                                                   (make_list(4,
                                                                              enc_sym_loop,
                                                                              mk_car(enc_sym_rst),
                                                                              mk_cdr(enc_sym_rst),
                                                                              body)))))),
                                  (make_list(4,
                                             enc_sym_loop,
                                             mk_car(lst),
                                             mk_cdr(lst),
                                             ENC_SYM_NIL)))));
}


void lbm_loop_extensions_init(void){
  lbm_add_symbol_const("a01", &sym_res);
  lbm_add_symbol_const("a02", &sym_loop);
  lbm_add_symbol_const("break", &sym_break);
  lbm_add_symbol_const("a03", &sym_brk);
  lbm_add_symbol_const("a04", &sym_rst);
  lbm_add_symbol_const("return", &sym_return);

  lbm_add_extension("me-loopfor", ext_me_loopfor);
  lbm_add_extension("me-loopwhile", ext_me_loopwhile);
  lbm_add_extension("me-looprange", ext_me_looprange);
  lbm_add_extension("me-loopforeach", ext_me_loopforeach);

}
