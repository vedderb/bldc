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

#ifdef LBM_USE_DYN_FUNS
static const char* lbm_dyn_fun[] = {
  "(defun str-merge () (str-join (rest-args)))",
  "(defun iota (n) (range n))",

  "(defun foldl (f init lst)"
  "(if (eq lst nil) init (foldl f (f init (car lst)) (cdr lst))))",

  "(defun foldr (f init lst)"
  "(if (eq lst nil) init (f (car lst) (foldr f init (cdr lst)))))",

  "(defun apply (f lst) (eval (cons f lst)))",

  "(defun zipwith (f xs ys) "
  "(let (( zip-acc (lambda (acc xs ys) "
  "(if (and xs ys) "
  "(zip-acc (cons (f (car xs) (car ys)) acc) (cdr xs) (cdr ys)) "
  "acc)))) "
  "(reverse (zip-acc nil xs ys))))",

  "(defun zip (xs ys) "
  "(zipwith cons xs ys))",

  "(defun filter (f lst)"
  "(let ((filter-rec (lambda (f lst ys)"
  "(if (eq lst nil)"
  "(reverse ys)"
  "(if (f (car lst))"
  "(filter-rec f (cdr lst) (cons (car lst) ys))"
  "(filter-rec f (cdr lst) ys))))))"
  "(filter-rec f lst nil)"
  "))",

  "(defun str-cmp-asc (a b) (< (str-cmp a b) 0))",
  "(defun str-cmp-dsc (a b) (> (str-cmp a b) 0))",

  "(defun second (x) (car (cdr x)))",
  "(defun third (x) (car (cdr (cdr x))))",

  "(defun abs (x) (if (< x 0) (- x) x))",
#ifdef LBM_USE_DYN_DEFSTRUCT
  "(defun create-struct (name num-fields) { "
  "(var arr (mkarray (+ 1 num-fields))) "
  "(setix arr 0 name) "
  "arr "
  "})",

  "(defun is-struct (struct name) "
  "(and (eq (type-of struct) type-lisparray) "
  "(eq (ix struct 0) name)))",

  "(defun accessor-sym (name field) "
  "(str2sym (str-merge name \"-\" (sym2str field))))",

  "(defun access-set (i) "
  "(lambda (struct) "
  "(if (rest-args) "
  "(setix struct i (rest-args 0)) "
  "(ix struct i)))) ",
#endif
};
#endif

#ifdef LBM_USE_DYN_MACROS
static const char* lbm_dyn_macros[] = {
  "(define defun (macro (name args body) (me-defun name args body)))",
  "(define defunret (macro (name args body) (me-defunret name args body)))",
  "(define defmacro (macro (name args body) `(define ,name (macro ,args ,body))))",
#ifdef LBM_USE_DYN_DEFSTRUCT
  "(define defstruct (macro (name list-of-fields)"
  "{"
  "(var num-fields (length list-of-fields))"
  "(var name-as-string (sym2str name))"
  "(var new-create-sym (str2sym (str-merge \"make-\" name-as-string)))"
  "(var new-pred-sym (str2sym (str-merge name-as-string \"?\")))"
  "(var field-ix (zip list-of-fields (range 1 (+ num-fields 1))))"
  "`(progn"
  "(define ,new-create-sym (lambda () (create-struct ',name ,num-fields)))"
  "(define ,new-pred-sym (lambda (struct) (is-struct struct ',name)))"
  ",@(map (lambda (x) (list define (accessor-sym name-as-string (car x))"
  "(access-set (cdr x)))) field-ix)"
  "'t"
  ")"
  "}))",

#endif
};

static lbm_uint sym_return;

static lbm_value ext_me_defun(lbm_value *argsi, lbm_uint argn) {
  if (argn != 3) {
    return ENC_SYM_EERROR;
  }

  lbm_value name = argsi[0];
  lbm_value args = argsi[1];
  lbm_value body = argsi[2];

  // (define name (lambda args body))

  return make_list(3,
                   ENC_SYM_DEFINE,
                   name,
                   mk_lam(args, body));
}

static lbm_value ext_me_defunret(lbm_value *argsi, lbm_uint argn) {
  if (argn != 3) {
    return ENC_SYM_EERROR;
  }

  lbm_value name = argsi[0];
  lbm_value args = argsi[1];
  lbm_value body = argsi[2];

  // (def name (lambda args (call-cc (lambda (return) body))))

  return make_list(3,
                   ENC_SYM_DEFINE,
                   name,
                   mk_lam(args,
                          mk_call_cc(mk_lam(make_list(1, lbm_enc_sym(sym_return)),
                                            body))));
}

#endif

void lbm_dyn_lib_init(void) {
#ifdef LBM_USE_DYN_MACROS
  lbm_add_symbol_const("return", &sym_return);

  lbm_add_extension("me-defun", ext_me_defun);
  lbm_add_extension("me-defunret", ext_me_defunret);
#endif
}

bool lbm_dyn_lib_find(const char *str, const char **code) {
#ifndef LBM_USE_DYN_MACROS
#ifndef LBM_USE_DYN_FUNS
  (void)str;
  (void)code;
#endif
#endif

#ifdef LBM_USE_DYN_MACROS
  for (unsigned int i = 0; i < (sizeof(lbm_dyn_macros) / sizeof(lbm_dyn_macros[0]));i++) {
    if (strmatch(str, lbm_dyn_macros[i] + 8)) { // define is 6 char
      *code = lbm_dyn_macros[i];
      return true;
    }
  }
#endif

#ifdef LBM_USE_DYN_FUNS
  for (unsigned int i = 0; i < (sizeof(lbm_dyn_fun) / sizeof(lbm_dyn_fun[0]));i++) {
    if (strmatch(str, lbm_dyn_fun[i] + 7)) { // defun is 5
      *code = lbm_dyn_fun[i];
      return true;
    }
  }
#endif
  return false;
}
