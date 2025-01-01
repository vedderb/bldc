;; Lisp in small pieces ch2 page 35.
;; Fun factoring.
;; Does not work on https://onecompiler.com/commonlisp

(define a (if 't (+ 3 4) (* 3 4)))
(define b (if nil (+ 3 4) (* 3 4)))
(define c ((if 't + *) 3 4))
(define d ((if nil + *) 3 4))

(print (and (eq a c 7)))
(print (and (eq b d 12)))
       
