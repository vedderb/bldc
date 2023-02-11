/*
    Copyright 2020, 2021, 2022 Joel Svensson  svenssonjoel@yahoo.se

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


#include <lbm_types.h>
#include "heap.h"
#include "symrepr.h"
#include "stack.h"
#include "qq_expand.h"

// defined in eval_cps.c
extern int lbm_perform_gc(void);

// Propagate an error outwards directly.
#define RET_ON_ERROR(y, x)       \
  (y) = (x);                     \
  if (lbm_is_error((y))) return y;

#define WITH_GC(y, x)              \
  (y) = (x);                       \
  if (lbm_is_symbol_merror((y))) { \
    lbm_perform_gc();              \
    (y) = (x);                     \
    if (lbm_is_error((y)))         \
      return (y);                  \
  }

#define WITH_GC_RMBR(y, x, n, ...)         \
  (y) = (x);                               \
  if (lbm_is_symbol_merror((y))) {         \
    lbm_gc_mark_phase((n), __VA_ARGS__);   \
    lbm_perform_gc();                      \
    (y) = (x);                             \
    if (lbm_is_error((y)))                 \
      return (y);                          \
  }

lbm_value quote_it(lbm_value qquoted) {
  if (lbm_is_symbol(qquoted) &&
      lbm_is_special(qquoted)) return qquoted;

  lbm_value val;
  WITH_GC_RMBR(val, lbm_cons(qquoted, ENC_SYM_NIL), 1, qquoted);
  lbm_value q;
  WITH_GC_RMBR(q, lbm_cons(ENC_SYM_QUOTE, val), 1, val);
  return q;
}


bool is_append(lbm_value a) {
  return (lbm_is_cons(a) &&
          lbm_is_symbol(lbm_car(a)) &&
          (lbm_dec_sym(lbm_car(a)) == SYM_APPEND));
}

lbm_value append(lbm_value front, lbm_value back) {
  if (lbm_is_symbol_nil(front)) return back;
  if (lbm_is_symbol_nil(back)) return front;

  if (lbm_is_quoted_list(front) &&
      lbm_is_quoted_list(back)) {
    lbm_value f = lbm_car(lbm_cdr(front));
    lbm_value b = lbm_car(lbm_cdr(back));
    return quote_it(lbm_list_append(f, b));
  }

  if (is_append(back) &&
      lbm_is_quoted_list(lbm_car(lbm_cdr(back))) &&
      lbm_is_quoted_list(front)) {
    lbm_value ql = lbm_car(lbm_cdr(back));
    lbm_value f = lbm_car(lbm_cdr(front));
    lbm_value b = lbm_car(lbm_cdr(ql));

    lbm_value v = lbm_list_append(f, b);
    lbm_set_car(lbm_cdr(ql), v);
    return back;
  }

  if (is_append(back)) {
    back  = lbm_cdr(back);
    lbm_value new;
    WITH_GC_RMBR(new, lbm_cons(front, back), 2, front, back);
    lbm_value tmp;
    WITH_GC_RMBR(tmp, lbm_cons(ENC_SYM_APPEND, new), 1, new);
    return tmp;
  }

  return lbm_cons (ENC_SYM_APPEND,
               lbm_cons(front,
                    lbm_cons(back, ENC_SYM_NIL)));
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
  lbm_value res;
  lbm_value car_val;
  lbm_value cdr_val;

  switch (lbm_type_of(l)) {
  case LBM_TYPE_CONS:
    car_val = lbm_car(l);
    cdr_val = lbm_cdr(l);
    if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(car_val) == SYM_COMMA) {
      lbm_value tl;
      WITH_GC(tl, lbm_cons(lbm_car(cdr_val), ENC_SYM_NIL));
      lbm_value tmp;
      WITH_GC_RMBR(tmp, lbm_cons(ENC_SYM_LIST, tl), 1, tl);
      res = tmp;
    } else if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
               lbm_dec_sym(car_val) == SYM_COMMAAT) {
      res = lbm_car(cdr_val);
    } else {
      lbm_value expand_car;
      RET_ON_ERROR(expand_car, qq_expand_list(car_val));
      lbm_value expand_cdr;
      RET_ON_ERROR(expand_cdr, lbm_qq_expand(cdr_val));

      lbm_value apnd;
      RET_ON_ERROR(apnd, append(expand_car, expand_cdr));

      lbm_value apnd_app;
      WITH_GC_RMBR(apnd_app, lbm_cons(apnd, ENC_SYM_NIL), 1, apnd);

      lbm_value tmp;
      WITH_GC_RMBR(tmp, lbm_cons(ENC_SYM_LIST, apnd_app), 1, apnd_app);
      res = tmp;
    }
    break;
  default: {
    lbm_value a_list;
    WITH_GC(a_list, lbm_cons(l, ENC_SYM_NIL));
    lbm_value tl;
    WITH_GC_RMBR(tl, lbm_cons(a_list, ENC_SYM_NIL), 1, a_list);
    lbm_value tmp;
    WITH_GC_RMBR(tmp, lbm_cons(ENC_SYM_QUOTE, tl), 1, tl);
    res = tmp;
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
  case LBM_TYPE_CONS:
    car_val = lbm_car(qquoted);
    cdr_val = lbm_cdr(qquoted);
    if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(car_val) == SYM_COMMA) {
      res = lbm_car(cdr_val);
    } else if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
               lbm_dec_sym(car_val) == SYM_COMMAAT) {
      res = ENC_SYM_RERROR; // should have a more specific error here.
    } else {
      lbm_value expand_car;
      RET_ON_ERROR(expand_car, qq_expand_list(car_val));
      lbm_value expand_cdr;
      RET_ON_ERROR(expand_cdr, lbm_qq_expand(cdr_val));
      res = append(expand_car, expand_cdr);
    }
    break;
  default:
    res = quote_it(qquoted);
    break;
  }
  return res;
}
