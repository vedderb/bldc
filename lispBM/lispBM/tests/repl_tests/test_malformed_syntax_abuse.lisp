; Test malformed expressions and syntax abuse to detect parser/evaluator vulnerabilities
; Goal: Ensure malformed expressions don't crash the evaluator (can return error, value, or nil)

; Test parser with various malformed expressions using read function
; We don't assume what will cause read errors - just test that nothing crashes
(define read_test1 (trap (read "(+ 1 2")))
(define read_test2 (trap (read "((+ 1 2)")))
(define read_test3 (trap (read "(+ 1 2))")))
(define read_test4 (trap (read "[1 2 3")))
(define read_test5 (trap (read "[| 1 2 3")))
(define read_test6 (trap (read "\"unclosed string")))
(define read_test7 (trap (read "'")))
(define read_test8 (trap (read "`")))
(define read_test9 (trap (read ",")))
(define read_test10 (trap (read ",@")))

(define read_test1_1 (trap (read "(+ 1 (+ 1 2)")))
(define read_test2_1 (trap (read "(\"string\" (+ 1 2)")))
(define read_test3_1 (trap (read "(+ 1 (+ 1 2)))")))
(define read_test4_1 (trap (read "(+ 1 [1 2 3")))
(define read_test5_1 (trap (read "(+ 1 [| 1 2 3")))
(define read_test6_1 (trap (read "(str-len \"unclosed string)")))
(define read_test7_1 (trap (read "''")))
(define read_test8_1 (trap (read "``")))
(define read_test9_1 (trap (read ",,")))
(define read_test10_1 (trap (read ",,@@")))


;; These strings may be too long for the reader before getting to exection of the "read" code
;; means this may lead to an untrappable error.
(define read_test1_2 (trap (read "(+ 10000000000000000000000000000 (+ 1 2)")))
;;(define read_test2_2 (trap (read "(\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" (+ 1 2)")))
(define read_test3_2 (trap (read "(+ 1 (+ 100000000000000000000000000000 2)))")))
(define read_test4_2 (trap (read "(+ 1000000000000000000000000000 [1 2 3")))
(define read_test5_2 (trap (read "(+ 1 [| 1 2 3")))
;;(define read_test6_2 (trap (read "(str-len \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)")))
(define read_test7_2 (trap (read "'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''")))
(define read_test8_2 (trap (read "```````````````````````````````````````````````````````````````")))
(define read_test9_2 (trap (read ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,")))
(define read_test10_2 (trap (read ",,@@")))

; Test malformed dotted pair syntax - may or may not cause read errors
(define read_test11 (trap (read "(+ 1 . 2)")))
(define read_test12 (trap (read "(or t . t)")))
(define read_test13 (trap (read "(. 1 2)")))
(define read_test14 (trap (read "(1 . 2 3)")))
(define read_test15 (trap (read "(1 .)")))

(define eval_dot_test11 (trap (+ 1 . 2)))
(define eval_dot_test12 (trap (or t . t)))
;; (define eval_dot_test13 (trap (. 1 2))) // This one is a read error
;; (define eval_dot_test14 (trap (1 . 2 3))) // This one is a read error
;; (define eval_dot_test15 (trap (1 .))) // This one is a read error 
(define eval_dot_test11_1 (trap (+ 1 . (+ 1 . 2))))
(define eval_dot_test12_1 (trap (and . (or t . t))))

; Test malformed brackets and braces
(define read_test16 (trap (read "{+ 1 2")))
(define read_test17 (trap (read "(+ 1 2}")))
(define read_test18 (trap (read "[1 2}")))
(define read_test19 (trap (read "{1 2]")))

; Test evaluator with parseable but potentially invalid expressions  
(define eval_test1 (trap (nonexistent_function 1 2 3)))
(define eval_test2 (trap (if)))
(define eval_test3 (trap (if 1)))
(define eval_test4 (trap (if 1 2 3 4)))
(define eval_test5 (trap (let)))
(define eval_test6 (trap (let ())))
(define eval_test7 (trap (define)))
(define eval_test8 (trap (lambda)))
(define eval_test9 (trap (lambda ())))
(define eval_test10 (trap (match)))

; Test arithmetic with wrong types
(define arith_test1 (trap (+ 'symbol 1)))
(define arith_test2 (trap (- "string" 1)))
(define arith_test3 (trap (* '(1 2) 3)))
(define arith_test4 (trap (/ t 2)))
(define arith_test5 (trap (mod nil 5)))

; Test list operations with wrong types
(define list_test1 (trap (car 42)))
(define list_test2 (trap (cdr "string")))
(define list_test3 (trap (cons 1)))
(define list_test4 (trap (append 42 '(1 2))))
(define list_test5 (trap (length t)))

; Test array access with edge cases (negative indices should index from end)
(define array_test1 (trap (ix [1 2 3] -1)))
(define array_test2 (trap (ix [1 2 3] 10)))
(define array_test3 (trap (ix "not-array" 0)))
(define array_test4 (trap (ix [1 2 3] "not-number")))

; Test string operations with invalid inputs
(define string_test1 (trap (str-from-n -1 10)))
(define string_test2 (trap (str-replace 42 "x" "y")))
(define string_test3 (trap (str-split "hello" 42)))

; Test symbol operations with wrong types
(define symbol_test1 (trap (sym2str 42)))
(define symbol_test2 (trap (str2sym 42)))

; Test buffer operations with wrong types
(define buffer_test1 (trap (bufclear 42)))
(define buffer_test2 (trap (bufget-u8 "not-buffer" 0)))

; Test deeply nested valid expressions (should not crash)
(define deep_test1 (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6))))))
(define deep_test2 (car (cdr (car (cdr '((1 2) (3 4)))))))

; Test malformed numeric literals through string conversion
(define num_test1 (to-i "not-a-number"))
(define num_test2 (to-float "invalid"))
(define num_test3 (to-i ""))
(define num_test4 (to-float ""))

; Test quote behavior - should not crash
(define quote_test1 (quote))
(define quote_test2 (quote a b))
(define quote_test3 (quote nil))
(define quote_test4 (quote t))

; Test function application with wrong arguments
(define apply_test1 (trap (apply + nil)))
(define apply_test2 (trap (apply 42 '(1 2))))
(define apply_test3 (trap (apply + "not-list")))

; Test basic evaluation - should not crash
(define safe_test1 (trap (eval nil)))
(define safe_test2 (trap (eval 42)))
(define safe_test3 (trap (eval "string")))
(define safe_test4 (trap (eval '())))

; Test edge cases that might cause issues
(define edge_test1 (trap (cdr nil)))
(define edge_test2 (trap (car nil)))
(define edge_test3 (trap (+ nil)))
(define edge_test4 (trap (- nil)))
(define edge_test5 (trap (* nil)))
(define edge_test6 (trap (/ nil)))

; Test type operations with various inputs
(define type_test1 (type-of nil))
(define type_test2 (type-of t))
(define type_test3 (type-of 'symbol))
(define type_test4 (type-of '()))
(define type_test5 (type-of (lambda (x) x)))

; Overall safety check - the test should complete without crashing
(print "SUCCESS: Malformed syntax abuse tests completed - no crashes detected")
