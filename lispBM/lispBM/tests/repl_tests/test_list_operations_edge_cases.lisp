; Test list operations with incorrect types and argument counts

; cons edge cases
(define cons_test1 (eq (trap (cons)) '(exit-error eval_error)))
(define cons_test2 (eq (trap (cons 1)) '(exit-error eval_error)))
(define cons_test3 (eq (car (cons 1 '(2 3))) 1))
(define cons_test4 (eq (trap (cons 1 2 3)) '(exit-error eval_error)))

; car edge cases
(define car_test1 (eq (trap (car)) '(exit-error eval_error)))
(define car_test2 (eq (trap (car "string")) '(exit-error type_error)))
(define car_test3 (eq (trap (car 42)) '(exit-error type_error)))
(define car_test4 (eq (trap (car 'symbol)) '(exit-error type_error)))
(define car_test5 (eq (car nil) nil))
(define car_test6 (eq (trap (car t)) '(exit-error type_error)))
(define car_test7 (eq (trap (car [1 2 3])) '(exit-error type_error)))
(define car_test8 (eq (trap (car 1 2)) '(exit-error eval_error)))

; cdr edge cases
(define cdr_test1 (eq (trap (cdr)) '(exit-error eval_error)))
(define cdr_test2 (eq (trap (cdr "string")) '(exit-error type_error)))
(define cdr_test3 (eq (trap (cdr 42)) '(exit-error type_error)))
(define cdr_test4 (eq (trap (cdr 'symbol)) '(exit-error type_error)))
(define cdr_test5 (eq (cdr nil) nil))
(define cdr_test6 (eq (trap (cdr t)) '(exit-error type_error)))
(define cdr_test7 (eq (trap (cdr [1 2 3])) '(exit-error type_error)))
(define cdr_test8 (eq (trap (cdr '(1) 2)) '(exit-error eval_error)))

; list edge cases
(define list_test1 (eq (list) nil))
(define list_test2 (eq (list 1) '(1)))
(define list_test3 (eq (list 1 2 3) '(1 2 3)))

; append edge cases
(define append_test1 (eq (append) nil))
(define append_test2 (eq (append '(1 2)) '(1 2)))
(define append_test3 (eq (trap (append "string" '(1 2))) '(exit-error type_error)))
(define append_test4 (eq (trap (append 42 '(1 2))) '(exit-error type_error)))
(define append_test5 (eq (append '(1 2) "string") '(1 2 . "string")))
(define append_test6 (eq (append '( 2) 42) '(2 . 42)))
(define append_test7 (eq (trap (append t '(1))) '(exit-error type_error)))
(define append_test8 (eq (append '(1) t) '(1 . t)))

; length edge cases (using list-length)
(define length_test1 (eq (trap (length)) '(exit-error eval_error)))
(define length_test2 (eq (length "string") 7))
(define length_test3 (eq (trap (length 42)) '(exit-error type_error)))
(define length_test4 (eq (trap (length 'symbol)) '(exit-error type_error)))
(define length_test5 (eq (length nil) 0))
(define length_test6 (eq (length '(1 2 3)) 3))
(define length_test7 (eq (trap (length t)) '(exit-error type_error)))
(define length_test8 (eq (length [1 2 3]) 3))
(define length_test9 (eq (trap (length '(1) 2)) '(exit-error eval_error)))

; ix (indexing) edge cases
(define ix_test1 (eq (trap (ix)) '(exit-error eval_error)))
(define ix_test2 (eq (trap (ix '(1 2 3))) '(exit-error eval_error)))
(define ix_test3 (eq (ix "string" 0) nil)) ;; TODO look into this
(define ix_test4 (eq (ix 42 0) nil)) ;; TODO look into this
(define ix_test5 (eq (trap (ix '(1 2 3) "string")) '(exit-error eval_error)))
(define ix_test6 (eq (trap (ix '(1 2 3) 'symbol)) '(exit-error eval_error)))
(define ix_test7 (eq (ix '(1 2 3) -1) 3))
(define ix_test8 (eq (ix '(1 2 3) 10)  nil))
(define ix_test9 (eq (ix '(1 2 3) 1) 2))
(define ix_test10 (eq(ix t 0) nil)) ;; TODO look into this

(if (and cons_test1 cons_test2 cons_test3 cons_test4
         car_test1 car_test2 car_test3 car_test4 car_test5 car_test6 car_test7 car_test8
         cdr_test1 cdr_test2 cdr_test3 cdr_test4 cdr_test5 cdr_test6 cdr_test7 cdr_test8
         list_test1 list_test2 list_test3
         append_test1 append_test2 append_test3 append_test4  append_test6 append_test7 append_test8
         append_test5
         length_test1 length_test2 length_test3 length_test4 length_test5 length_test6 length_test7 length_test8 length_test9
         ix_test1 ix_test2 ix_test3 ix_test4 ix_test5 ix_test6 ix_test7 ix_test8 ix_test9 ix_test10)
    (print "SUCCESS")
    (print "FAILURE"))
