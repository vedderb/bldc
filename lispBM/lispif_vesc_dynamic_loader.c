/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "lispif.h"
#include "lispbm.h"

static const char* functions[] = {
"(defun uart-read-bytes (buffer n ofs)"
"(let ((rd (uart-read buffer n ofs)))"
"(if (= rd n)"
"(bufset-u8 buffer (+ ofs rd) 0)"
"(progn (yield 4000) (uart-read-bytes buffer (- n rd) (+ ofs rd)))"
")))",

"(defun uart-read-until (buffer n ofs end)"
"(let ((rd (uart-read buffer n ofs end)))"
"(if (and (> rd 0) (or (= rd n) (= (bufget-u8 buffer (+ ofs (- rd 1))) end)))"
"(bufset-u8 buffer (+ ofs rd) 0)"
"(progn (yield 10000) (uart-read-until buffer (- n rd) (+ ofs rd) end))"
")))",

"(defun iota (n) (range n))",

"(defun foldl (f init lst)"
"(if (eq lst nil) init (foldl f (f init (car lst)) (cdr lst))))",

"(defun foldr (f init lst)"
"(if (eq lst nil) init (f (car lst) (foldr f init (cdr lst)))))",

"(defun apply (f lst) (eval (cons f lst)))",

"(defun zipwith (f x y)"
"(let ((map-rec (lambda (f res lst ys)"
"(if (eq lst nil)"
"(reverse res)"
"(map-rec f (cons (f (car lst) (car ys)) res) (cdr lst) (cdr ys))))))"
"(map-rec f nil x y)))",

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

static const char* macros[] = {
"(define defun (macro (name args body) (me-defun name args body)))",
"(define defunret (macro (name args body) (me-defunret name args body)))",
"(define loopfor (macro (it start cnd update body) (me-loopfor it start cnd update body)))",
"(define loopwhile (macro (cnd body) (me-loopwhile cnd body)))",
"(define looprange (macro (it start end body) (me-looprange it start end body)))",
"(define loopforeach (macro (it lst body) (me-loopforeach it lst body)))",
"(define loopwhile-thd (macro (stk cnd body) `(spawn ,stk (fn () (loopwhile ,cnd ,body)))))",
};

static bool strmatch(const char *str1, const char *str2) {
	unsigned int len = strlen(str1);

	if (str2[len] != ' ') {
		return false;
	}

	bool same = true;
	for (unsigned int i = 0;i < len;i++) {
		if (str1[i] != str2[i]) {
			same = false;
			break;
		}
	}

	return same;
}

bool lispif_vesc_dynamic_loader(const char *str, const char **code) {
	for (unsigned int i = 0; i < (sizeof(macros) / sizeof(macros[0]));i++) {
		if (strmatch(str, macros[i] + 8)) {
			*code = macros[i];
			return true;
		}
	}

	for (unsigned int i = 0; i < (sizeof(functions) / sizeof(functions[0]));i++) {
		if (strmatch(str, functions[i] + 7)) {
			*code = functions[i];
			return true;
		}
	}

	return false;
}
