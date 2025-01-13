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

#ifndef LBM_DYN_LIB_H_
#define LBM_DYN_LIB_H_

const char* lbm_dyn_fun[] = {
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
};

const char* lbm_dyn_macros[] =
  {
  "(define defun "
  "(macro (name param body) "
  "`(define ,name (lambda ,param ,body))))",

  "(define defunret "
  "(macro (name param body) "
  "`(define ,name (lambda ,param (call-cc-unsafe (lambda (return) ,body))))))"

  };

#endif
