; Test comparison operations with incorrect types and argument counts

; Equality (=) edge cases  
(define eq_test1 (eq (trap (=)) '(exit-error type_error)))
(define eq_test2 (eq (= 5) 't))
(define eq_test3 (eq (trap (= "string" 2)) '(exit-error type_error)))
(define eq_test4 (eq (trap (= 5 'symbol)) '(exit-error type_error)))
(define eq_test5 (eq (trap (= nil 0)) '(exit-error type_error)))
(define eq_test6 (eq (trap (= '(1 2) 3)) '(exit-error type_error)))
(define eq_test7 (eq (trap (= t 1)) '(exit-error type_error)))
(define eq_test8 (eq (trap (= [1] 1)) '(exit-error type_error)))

; Numeric inequality (!=) edge cases
(define neq_test1 (eq (trap (!=)) '(exit-error type_error)))
(define neq_test2 (eq (!= 5) nil))
(define neq_test3 (eq (trap (!= "string" 2)) '(exit-error type_error)))
(define neq_test4 (eq (trap (!= 5 'symbol)) '(exit-error type_error)))
(define neq_test5 (eq (trap (!= nil 0)) '(exit-error type_error)))
(define neq_test6 (eq (trap (!= '(1) 1)) '(exit-error type_error)))
(define neq_test7 (eq (trap (!= t 1)) '(exit-error type_error)))
(define neq_test8 (eq (trap (!= [1] 1)) '(exit-error type_error)))

; Less than (<) edge cases
(define lt_test1 (eq (trap (<)) '(exit-error type_error)))
(define lt_test2 (eq (< 5) nil))
(define lt_test3 (eq (trap (< "string" 2)) '(exit-error type_error)))
(define lt_test4 (eq (trap (< 5 'symbol)) '(exit-error type_error)))
(define lt_test5 (eq (trap (< nil 0)) '(exit-error type_error)))
(define lt_test6 (eq (trap (< '(1) 2)) '(exit-error type_error)))
(define lt_test7 (eq (trap (< t 1)) '(exit-error type_error)))
(define lt_test8 (eq (trap (< [1] 2)) '(exit-error type_error)))

; Greater than (>) edge cases
(define gt_test1 (eq (trap (>)) '(exit-error type_error)))
(define gt_test2 (eq (> 5) nil))
(define gt_test3 (eq (trap (> "string" 2)) '(exit-error type_error)))
(define gt_test4 (eq (trap (> 5 'symbol)) '(exit-error type_error)))
(define gt_test5 (eq (trap (> nil 0)) '(exit-error type_error)))
(define gt_test6 (eq (trap (> '(5) 2)) '(exit-error type_error)))
(define gt_test7 (eq (trap (> t 1)) '(exit-error type_error)))
(define gt_test8 (eq (trap (> [5] 2)) '(exit-error type_error)))

; Less than or equal (<=) edge cases
(define leq_test1 (eq (trap (<=)) '(exit-error type_error)))
(define leq_test2 (eq (<= 5) 't))
(define leq_test3 (eq (trap (<= "string" 2)) '(exit-error type_error)))
(define leq_test4 (eq (trap (<= 5 'symbol)) '(exit-error type_error)))
(define leq_test5 (eq (trap (<= nil 0)) '(exit-error type_error)))
(define leq_test6 (eq (trap (<= '(1) 2)) '(exit-error type_error)))
(define leq_test7 (eq (trap (<= t 1)) '(exit-error type_error)))
(define leq_test8 (eq (trap (<= [1] 2)) '(exit-error type_error)))

; Greater than or equal (>=) edge cases
(define geq_test1 (eq (trap (>=)) '(exit-error type_error)))
(define geq_test2 (eq (>= 5) 't))
(define geq_test3 (eq (trap (>= "string" 2)) '(exit-error type_error)))
(define geq_test4 (eq (trap (>= 5 'symbol)) '(exit-error type_error)))
(define geq_test5 (eq (trap (>= nil 0)) '(exit-error type_error)))
(define geq_test6 (eq (trap (>= '(5) 2)) '(exit-error type_error)))
(define geq_test7 (eq (trap (>= t 1)) '(exit-error type_error)))
(define geq_test8 (eq (trap (>= [5] 2)) '(exit-error type_error)))

(if (and eq_test1 eq_test2 eq_test3 eq_test4 eq_test5 eq_test6 eq_test7 eq_test8
         neq_test1 neq_test2 neq_test3 neq_test4 neq_test5 neq_test6 neq_test7 neq_test8
         lt_test1 lt_test2 lt_test3 lt_test4 lt_test5 lt_test6 lt_test7 lt_test8
         gt_test1 gt_test2 gt_test3 gt_test4 gt_test5 gt_test6 gt_test7 gt_test8
         leq_test1 leq_test2 leq_test3 leq_test4 leq_test5 leq_test6 leq_test7 leq_test8
         geq_test1 geq_test2 geq_test3 geq_test4 geq_test5 geq_test6 geq_test7 geq_test8)
    (print "SUCCESS")
    (print "FAILURE"))
