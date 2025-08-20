; Test merge function with edge cases and incorrect arguments

; merge with valid arguments
(define merge_test1 (eq (merge < '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6)))
(define merge_test2 (eq (merge < '() '(1 2 3)) '(1 2 3)))
(define merge_test3 (eq (merge < '(1 2 3) '()) '(1 2 3)))
(define merge_test4 (eq (merge < '() '()) '()))
(define merge_test5 (eq (merge < '(1) '(2)) '(1 2)))
(define merge_test6 (eq (merge > '(5 3 1) '(6 4 2)) '(6 5 4 3 2 1)))

; merge with invalid argument count
(define merge_test7 (eq (trap (merge)) '(exit-error type_error)))
(define merge_test8 (eq (trap (merge <)) '(exit-error type_error)))
(define merge_test9 (eq (trap (merge < '(1 2))) '(exit-error type_error)))
(define merge_test10 (eq (trap (merge < '(1 2) '(3 4) '(5 6))) '(exit-error type_error)))

; merge with invalid comparator
(define merge_test11 (eq (trap (merge "not-function" '(1 2) '(3 4))) '(exit-error eval_error)))
(define merge_test12 (eq (trap (merge 42 '(1 2) '(3 4))) '(exit-error eval_error)))
(define merge_test13 (eq (trap (merge 'symbol '(1 2) '(3 4))) '(exit-error variable_not_bound)))

; merge with invalid list arguments
(define merge_test14 (eq (trap (merge < "not-list" '(1 2))) '(exit-error type_error)))
(define merge_test15 (eq (trap (merge < '(1 2) "not-list")) '(exit-error type_error)))
(define merge_test16 (eq (trap (merge < 42 '(1 2))) '(exit-error type_error)))
(define merge_test17 (eq (trap (merge < '(1 2) 42)) '(exit-error type_error)))
(define merge_test18 (eq (trap (merge < 'symbol '(1 2))) '(exit-error type_error)))
(define merge_test19 (eq (trap (merge < '(1 2) 'symbol)) '(exit-error type_error)))
(define merge_test20 (eq (trap (merge < t '(1 2))) '(exit-error type_error)))
(define merge_test21 (eq (trap (merge < '(1 2) t)) '(exit-error type_error)))

; merge with arrays (should fail)
(define merge_test22 (eq (trap (merge < [1 2 3] '(4 5 6))) '(exit-error type_error)))
(define merge_test23 (eq (trap (merge < '(1 2 3) [4 5 6])) '(exit-error type_error)))
(define merge_test24 (eq (trap (merge < [| 1 2 3 |] '(4 5 6))) '(exit-error type_error)))

; merge with mixed types in lists (should work if comparator handles it)
(define mixed_cmp (lambda (x y) (< (to-i x) (to-i y))))
(define merge_test25 (eq (merge mixed_cmp '(1 2) '(3.0 4.0)) '(1 2 3.0 4.0)))

; merge with string elements
(define str_cmp (lambda (x y) (< (str-cmp x y) 0)))
(define merge_test26 (eq (merge str_cmp '("a" "c") '("b" "d")) '("a" "b" "c" "d")))


(if (and merge_test1 merge_test2 merge_test3 merge_test4 merge_test5 merge_test6
        merge_test7 merge_test8 merge_test9 merge_test10
        merge_test11 merge_test12 merge_test13
        merge_test14 merge_test15 merge_test16 merge_test17 merge_test18 merge_test19 merge_test20 merge_test21
        merge_test22 merge_test23 merge_test24
        merge_test25 merge_test26)
    (print "SUCCESS")
    (print "FAILURE"))
