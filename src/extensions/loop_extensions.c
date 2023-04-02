/*
    Copyright 2023 Joel Svensson        svenssonjoel@yahoo.se
              2022 Benjamin Vedder      benjamin@vedder.se

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
#include <stdarg.h>

static lbm_uint sym_res;
static lbm_uint sym_loop;
static lbm_uint sym_break;
static lbm_uint sym_brk;
static lbm_uint sym_rst;

static lbm_value make_list(unsigned int n, ...) {
  lbm_value res;
  va_list valist;
  va_start(valist, n);
  res = lbm_heap_allocate_list_init_va(n, valist);
  va_end(valist);
  return res;
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

  // (let ((loop (lambda (it res break) (if cond (loop update body break) res)))) (call-cc (lambda (brk) (loop start nil brk))))
  
   return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(3, it, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           cond,
                                                           make_list(4, lbm_enc_sym(sym_loop), update, body, lbm_enc_sym(sym_break)),
                                                           lbm_enc_sym(sym_res))))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(4, lbm_enc_sym(sym_loop), start, ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_loopwhile(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    return ENC_SYM_EERROR;
  }

  lbm_value cond = args[0];
  lbm_value body = args[1];

  // (let ((loop (lambda (res break) (if cond (loop body break) res)))) (call-cc (lambda (brk) (loop nil brk))))

  return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(2, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           cond,
                                                           make_list(3, lbm_enc_sym(sym_loop), body, lbm_enc_sym(sym_break)),
                                                           lbm_enc_sym(sym_res))))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(3, lbm_enc_sym(sym_loop), ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_looprange(lbm_value *args, lbm_uint argn) {
  if (argn != 4) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value start = args[1];
  lbm_value end = args[2];
  lbm_value body = args[3];

  // (let ((loop (lambda (it res break) (if (< it end) (loop (+ it 1) body break) res)))) (call-cc (lambda (brk) (loop start nil brk))))

  return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(3, it, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           make_list(3, lbm_enc_sym(SYM_LT), it, end),
                                                           make_list(4, lbm_enc_sym(sym_loop), make_list(3, lbm_enc_sym(SYM_ADD), it, lbm_enc_i(1)), body, lbm_enc_sym(sym_break)),
                                                           lbm_enc_sym(sym_res))))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(4, lbm_enc_sym(sym_loop), start, ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_loopforeach(lbm_value *args, lbm_uint argn) {
  if (argn != 3) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value lst = args[1];
  lbm_value body = args[2];

  // (let ((loop (lambda (it rst res break) (if (eq it nil) res (loop (car rst) (cdr rst) body break))))) (call-cc (lambda (brk) (loop (car lst) (cdr lst) nil brk))))

  return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(4, it, lbm_enc_sym(sym_rst), lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           make_list(3, lbm_enc_sym(SYM_EQ), it, ENC_SYM_NIL),
                                                           lbm_enc_sym(sym_res),
                                                           make_list(5,
                                                                     lbm_enc_sym(sym_loop),
                                                                     make_list(2, lbm_enc_sym(SYM_CAR), lbm_enc_sym(sym_rst)),
                                                                     make_list(2, lbm_enc_sym(SYM_CDR), lbm_enc_sym(sym_rst)),
                                                                     body,
                                                                     lbm_enc_sym(sym_break))
                                                           )))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(5,
                                                 lbm_enc_sym(sym_loop),
                                                 make_list(2, lbm_enc_sym(SYM_CAR), lst),
                                                 make_list(2, lbm_enc_sym(SYM_CDR), lst),
                                                 ENC_SYM_NIL,
                                                 lbm_enc_sym(sym_brk)))));
}

const char *loop_extensions_dyn_load[4] = {
  "(define loopfor (macro (it start cnd update body) (me-loopfor it start cnd update body)))",
  "(define loopwhile (macro (cnd body) (me-loopwhile cnd body)))",
  "(define looprange (macro (it start end body) (me-looprange it start end body)))",
  "(define loopforeach (macro (it lst body) (me-loopforeach it lst body)))"
};

bool lbm_loop_extensions_init(void) {

  lbm_add_symbol_const("a01", &sym_res);
  lbm_add_symbol_const("a02", &sym_loop);
  lbm_add_symbol_const("break", &sym_break);
  lbm_add_symbol_const("a03", &sym_brk);
  lbm_add_symbol_const("a04", &sym_rst);

  bool res = true;
  res = res && lbm_add_extension("me-loopfor", ext_me_loopfor);
  res = res && lbm_add_extension("me-loopwhile", ext_me_loopwhile);
  res = res && lbm_add_extension("me-looprange", ext_me_looprange);
  res = res && lbm_add_extension("me-loopforeach", ext_me_loopforeach);
  return res;
}
