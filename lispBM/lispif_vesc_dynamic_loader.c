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
"(if (num-eq rd n)"
"(bufset-u8 buffer 0 (+ ofs rd))"
"(progn (yield 4000) (uart-read-bytes buffer (- n rd) (+ ofs rd)))"
")))",

"(defun uart-read-until (buffer n ofs end)"
"(let ((rd (uart-read buffer n ofs end)))"
"(if (and (> rd 0) (or (num-eq rd n) (num-eq (bufget-u8 buffer (+ ofs (- rd 1))) end)))"
"(bufset-u8 buffer 0 (+ ofs rd))"
"(progn (yield 10000) (uart-read-until buffer (- n rd) (+ ofs rd) end))"
")))",

"(defun map (f xs)"
"(if (= xs nil) nil "
"(cons (f (car xs)) (map f (cdr xs)))))",

"(defun iota (n)"
"(let ((iacc (lambda (acc i)"
"(if (< i 0) acc (iacc (cons i acc) (- i 1))))))"
"(iacc nil n)))",

"(defun range (start end)"
"(map (lambda (x) (+ x start)) (iota (- end start))))",

"(defun foldl (f i xs)"
"(if (= xs nil) i (foldl f (f i (car xs)) (cdr xs))))",

"(defun foldr (f i xs)"
"(if (= xs nil) i (f (car xs) (foldr f i (cdr xs)))))",

"(defun reverse (xs)"
"(let ((revacc (lambda (acc xs)"
"(if (= nil xs) acc (revacc (cons (car xs) acc) (cdr xs))))))"
"(revacc nil xs)))",

"(defun length (xs)"
"(let ((len (lambda (l xs)"
"(if (= xs nil) l (len (+ l 1) (cdr xs))))))"
"(len 0 xs)))",

"(defun apply (fun lst) (eval `(,fun ,@lst)))",

"(defun zipwith (f x y)"
"(let ((map-rec (lambda (f res xs ys)"
"(if (= xs nil)"
"(reverse res)"
"(map-rec f (cons (f (car xs) (car ys)) res) (cdr xs) (cdr ys))))))"
"(map-rec f nil x y)))",
};

static const char* macros[] = {
"(define defun (macro (name args body) `(define ,name (lambda ,args ,body))))",
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
