/*
    Copyright 2020, 2021 Joel Svensson  svenssonjoel@yahoo.se

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


/*
   Quasiquotation expansion attempt

   Trying to adapt the algorithm from Bawden - Quasiquotation in Lisp
   to be applied during parsing.
*/


#include "heap.h"
#include "lispbm_types.h"
#include "symrepr.h"
#include "stack.h"
#include "qq_expand.h"


lbm_value gen_cons(lbm_value a, lbm_value b) {
  return lbm_cons(lbm_enc_sym(SYM_CONS),
                   lbm_cons(a,
                        lbm_cons(b, lbm_enc_sym(SYM_NIL))));
}


lbm_value append(lbm_value front, lbm_value back) {
  return lbm_cons (lbm_enc_sym(SYM_APPEND),
               lbm_cons(front,
                    lbm_cons(back, lbm_enc_sym(SYM_NIL))));
}

/* Bawden's qq-expand-list implementation
(define (qq-expand-list x)
  (cond ((tag-comma? x)
         `(list ,(tag-data x)))
        ((tag-comma-atsign? x)
         (tag-data x))
        ((tag-backquote? x)
         (qq-expand-list
          (qq-expand (tag-data x))))
        ((pair? x)
         `(list
           (append
            ,(qq-expand-list (car x))
            ,(qq-expand (cdr x)))))
        (else `'(,x))))
*/

lbm_value qq_expand_list(lbm_value l) {
  lbm_value res = lbm_enc_sym(SYM_NIL);
  lbm_value car_val;
  lbm_value cdr_val;

  switch (lbm_type_of(l)) {
  case LBM_PTR_TYPE_CONS:
    car_val = lbm_car(l);
    cdr_val = lbm_cdr(l);
    if (lbm_type_of(car_val) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(car_val) == SYM_COMMA) {
      res = lbm_cons(lbm_enc_sym(SYM_LIST),
                 lbm_cons(lbm_car(cdr_val), res));
    } else if (lbm_type_of(car_val) == LBM_VAL_TYPE_SYMBOL &&
               lbm_dec_sym(car_val) == SYM_COMMAAT) {
      res = lbm_car(cdr_val);
    } else {
      lbm_value expand_car = qq_expand_list(car_val);
      lbm_value expand_cdr = lbm_qq_expand(cdr_val);
      res = lbm_cons(lbm_enc_sym(SYM_LIST),
                 lbm_cons(append(expand_car, expand_cdr), lbm_enc_sym(SYM_NIL)));
    }
    break;
  default: {
    lbm_value a_list = lbm_cons(l, lbm_enc_sym(SYM_NIL));
    res =
      lbm_cons(lbm_enc_sym(SYM_QUOTE), lbm_cons (a_list, lbm_enc_sym(SYM_NIL)));
  }
  }
  return res;
}

/* Bawden's qq-expand implementation
(define (qq-expand x)
  (cond ((tag-comma? x)
         (tag-data x))
        ((tag-comma-atsign? x)
         (error "Illegal"))
        ((tag-backquote? x)
         (qq-expand
          (qq-expand (tag-data x))))
        ((pair? x)
         `(append
           ,(qq-expand-list (car x))
           ,(qq-expand (cdr x))))
        (else `',x)))
 */

lbm_value lbm_qq_expand(lbm_value qquoted) {

  lbm_value res;
  lbm_value car_val;
  lbm_value cdr_val;

  switch (lbm_type_of(qquoted)) {
  case LBM_PTR_TYPE_CONS:
    car_val = lbm_car(qquoted);
    cdr_val = lbm_cdr(qquoted);
    if (lbm_type_of(car_val) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(car_val) == SYM_COMMA) {
      res = lbm_car(cdr_val);
    } else if (lbm_type_of(car_val) == LBM_VAL_TYPE_SYMBOL &&
               lbm_dec_sym(car_val) == SYM_COMMAAT) {
      res = lbm_enc_sym(SYM_RERROR); // should have a more specific error here. 
    } else {
      lbm_value expand_car = qq_expand_list(car_val);
      lbm_value expand_cdr = lbm_qq_expand(cdr_val);
      res = append(expand_car, expand_cdr);
    }
    break;
  default:
    res = lbm_cons(lbm_enc_sym(SYM_QUOTE), lbm_cons(qquoted, lbm_enc_sym(SYM_NIL)));
    break;
  }
  return res;
}
