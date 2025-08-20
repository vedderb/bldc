; Test arithmetic operations with incorrect types and argument counts

; Addition edge cases
(define add_test1 (eq (+ 0) 0))
(define add_test2 (eq (+ 1) 1))
(define add_test3 (eq (trap (+ "string" 2)) '(exit-error type_error)))
(define add_test4 (eq (trap (+ 1 'symbol)) '(exit-error type_error)))
(define add_test5 (eq (trap (+ nil 5)) '(exit-error type_error)))
(define add_test6 (eq (trap (+ '(1 2) 3)) '(exit-error type_error)))
(define add_test7 (eq (trap (+ t 1)) '(exit-error type_error)))
(define add_test8 (eq (trap (+ [1 2 3] 2)) '(exit-error type_error)))

; Subtraction edge cases
(define sub_test1 (eq (- 0) 0))
(define sub_test2 (eq (- 5) -5))
(define sub_test3 (eq (trap (- "string" 2)) '(exit-error type_error)))
(define sub_test4 (eq (trap (- 10 'symbol)) '(exit-error type_error)))
(define sub_test5 (eq (trap (- nil 3)) '(exit-error type_error)))
(define sub_test6 (eq (trap (- '(1 2) 1)) '(exit-error type_error)))
(define sub_test7 (eq (trap (- 5 t)) '(exit-error type_error)))
(define sub_test8 (eq (trap (- [1 2] 1)) '(exit-error type_error)))

; Multiplication edge cases
(define mul_test1 (eq (* 1) 1))
(define mul_test2 (eq (* 7) 7))
(define mul_test3 (eq (trap (* "string" 2)) '(exit-error type_error)))
(define mul_test4 (eq (trap (* 3 'symbol)) '(exit-error type_error)))
(define mul_test5 (eq (trap (* nil 2)) '(exit-error type_error)))
(define mul_test6 (eq (trap (* '(1) 3)) '(exit-error type_error)))
(define mul_test7 (eq (trap (* t 4)) '(exit-error type_error)))
(define mul_test8 (eq (trap (* [1] 2)) '(exit-error type_error)))

; Division edge cases
(define div_test1 (eq (trap (/ 1)) '(exit-error eval_error)))
(define div_test2 (eq (trap (/ 10)) '(exit-error eval_error)))
(define div_test3 (eq (trap (/ "string" 2)) '(exit-error type_error)))
(define div_test4 (eq (trap (/ 10 'symbol)) '(exit-error type_error)))
(define div_test5 (eq (trap (/ nil 2)) '(exit-error type_error)))
(define div_test6 (eq (trap (/ '(10) 2)) '(exit-error type_error)))
(define div_test7 (eq (trap (/ 8 t)) '(exit-error type_error)))
(define div_test8 (eq (trap (/ [10] 2)) '(exit-error type_error)))
(define div_test9 (eq (trap (/ 5 0)) '(exit-error division_by_zero)))

; Modulo edge cases  
(define mod_test1 (eq (trap (mod 7)) '(exit-error eval_error)))
(define mod_test2 (eq (mod 7 3) 1))
(define mod_test3 (eq (trap (mod "string" 2)) '(exit-error type_error)))
(define mod_test4 (eq (trap (mod 10 'symbol)) '(exit-error type_error)))
(define mod_test5 (eq (trap (mod nil 3)) '(exit-error type_error)))
(define mod_test6 (eq (trap (mod '(7) 2)) '(exit-error type_error)))
(define mod_test7 (eq (trap (mod 7 t)) '(exit-error type_error)))
(define mod_test8 (eq (trap (mod [7] 2)) '(exit-error type_error)))
(define mod_test9 (eq (trap (mod 7 0)) '(exit-error division_by_zero)))

(if (and add_test1 add_test2 add_test3 add_test4 add_test5 add_test6 add_test7 add_test8
         sub_test1 sub_test2 sub_test3 sub_test4 sub_test5 sub_test6 sub_test7 sub_test8
         mul_test1 mul_test2 mul_test3 mul_test4 mul_test5 mul_test6 mul_test7 mul_test8
         div_test1 div_test2 div_test3 div_test4 div_test5 div_test6 div_test7 div_test8 div_test9
         mod_test1 mod_test2 mod_test3 mod_test4 mod_test5 mod_test6 mod_test7 mod_test8 mod_test9)
    (print "SUCCESS")
    (print "FAILURE"))
