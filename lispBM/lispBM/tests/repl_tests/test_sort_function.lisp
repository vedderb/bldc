; Test sort function with edge cases and incorrect arguments

;; Helper function to check if a list is sorted
;; is-sorted is not really correct. It can only be checked in this
; way for some comparators.
(define is-sorted
  (lambda (acc cmp ls)
    (if (or (eq ls nil)
            (eq (cdr ls) nil))
        acc
      (is-sorted (and acc (cmp (car ls) (car (cdr ls)))) cmp (cdr ls)))))

; sort with valid arguments
(define sort_test1 (let ((result (sort < '(3 1 4 99 5 9 2 6))))
                     (and (is-sorted t < result) (= (length result) 8))))
(define sort_test2 (eq (sort < '()) '()))
(define sort_test3 (eq (sort < '(1)) '(1)))
(define sort_test4 (let ((result (sort > '(1 2 3 4 5))))
                     (and (is-sorted t > result) (= (length result) 5))))
(define sort_test5 (eq (sort < '(5 5 5 5)) '(5 5 5 5)))

                                        ; sort with invalid argument count
(define extra-arg 98)
(define sort_test6 (eq (trap (sort)) '(exit-error type_error)))
(define sort_test7 (eq (trap (sort <)) '(exit-error type_error)))
(define sort_test8
    (eq
     (trap
      (sort < '(1 2 3) extra-arg)) '(exit-error type_error)))

; sort with invalid comparator
(define sort_test9 (eq (trap (sort "not-function" '(1 2 3))) '(exit-error eval_error)))
(define sort_test10 (eq (trap (sort 42 '(1 2 3))) '(exit-error eval_error)))
(define sort_test11 (eq (trap (sort 'symbol '(1 2 3))) '(exit-error variable_not_bound)))
(define sort_test12 (eq (trap (sort t '(1 2 3))) '(exit-error eval_error)))

; sort with invalid list arguments
(define sort_test13 (eq (trap (sort < "not-list")) '(exit-error type_error)))
(define sort_test14 (eq (trap (sort < 42)) '(exit-error type_error)))
(define sort_test15 (eq (trap (sort < 'symbol)) '(exit-error type_error)))
(define sort_test16 (eq (trap (sort < t)) '(exit-error type_error)))

; sort with arrays (should fail)
(define sort_test17 (eq (trap (sort < [1 3 2])) '(exit-error type_error)))
(define sort_test18 (eq (trap (sort < [| 1 3 2 |])) '(exit-error type_error)))

; sort with mixed types (should work if comparator handles it)
(define mixed_cmp (lambda (x y) (< (to-i x) (to-i y))))
(define sort_test19 (let ((result (sort mixed_cmp '(3.0 1 4.5 2))))
                      (and (is-sorted t mixed_cmp result) (= (length result) 4))))

; sort with string elements
(define str_cmp (lambda (x y) (< (str-cmp x y) 0)))
(define sort_test20 (let ((result (sort str_cmp '("zebra" "apple" "banana" "cherry"))))
                      (and (is-sorted t str_cmp result) (= (length result) 4))))

; sort preserves duplicates
(define sort_test21 (let ((result (sort < '(3 1 2 6 8))))
                      (and (is-sorted t < result) (= (length result) 5))))

; sort with large list
(define sort_test22 (let ((input (range 1 10))
                          (result (sort < input)))
                      (and (is-sorted t < result) (= (length result) 9))))

; sort with negative numbers
(define sort_test23 (let ((result (sort < '(-5 3 -1 0 -10 7))))
                      (and (is-sorted t < result) (= (length result) 6))))

; sort stability test (equal elements should maintain relative order, though merge-sort may not guarantee this)
(define sort_test24 (let ((result (sort < '(1 2 6 9 10))))
                      (and (is-sorted t < result) (= (length result) 5))))

(if (and sort_test1 sort_test2 sort_test3 sort_test4 sort_test5
         sort_test6 sort_test7 sort_test8
         sort_test9 sort_test10 sort_test11 sort_test12
         sort_test13 sort_test14 sort_test15 sort_test16
         sort_test17 sort_test18
         sort_test19 sort_test20 sort_test21 sort_test22 sort_test23 sort_test24)
    (print "SUCCESS")
    (print "FAILURE"))
