; Runtime extension edge cases test

; Basic function tests
(define test1 (number? (mem-num-free)))
(define test2 (list? (lbm-version)))
(define test3 (eq (set-eval-quota 1000) t))
(define test4 (eq (trap (set-eval-quota)) '(exit-error eval_error)))

; lbm-heap-state with valid symbol argument
(define heap_test1 (number? (lbm-heap-state 'get-heap-size)))
(define heap_test2 (number? (lbm-heap-state 'get-heap-bytes)))
(define heap_test9 (number? (lbm-heap-state 'get-num-alloc-arrays)))

; lbm-heap-state with 0 arguments
(define heap_test3 (eq (trap (lbm-heap-state)) '(exit-error type_error)))

; lbm-heap-state with more than 1 argument  
(define heap_test4 (eq (trap (lbm-heap-state 'get-heap-size 'extra)) '(exit-error type_error)))
(define heap_test5 (eq (trap (lbm-heap-state 'get-heap-size 42 "string")) '(exit-error type_error)))

; lbm-heap-state with non-symbol argument
(define heap_test6 (eq (trap (lbm-heap-state 42)) '(exit-error type_error)))
(define heap_test7 (eq (trap (lbm-heap-state "string")) '(exit-error type_error)))

; lbm-heap-state with invalid symbol (should return nil, not error)
(define heap_test8 (eq (lbm-heap-state 'invalid-symbol) nil))

(define env_test1 (eq (trap (env-get)) '(exit-error type_error)))
(define env_test2 (eq (env-set) nil))
(define env_test3 (eq (env-set 'apa 1) nil))
(define env_test4 (eq (trap (env-get 'apa)) '(exit-error type_error)))

(define stack_test1 (eq (trap (set-gc-stack-size 'apa)) '(exit-error type_error)))
(define stack_test2 (eq (trap (set-gc-stack-size 1000000)) '(exit-error out_of_memory)))
(define stack_test3 (eq (trap (set-gc-stack-size)) '(exit-error type_error)))

(if (and test1 test2 test3 test4 
         heap_test1 heap_test2 heap_test3 heap_test4 heap_test5 
         heap_test6 heap_test7 heap_test8
         heap_test9
         env_test1 env_test2 env_test3 env_test4
         stack_test1 stack_test2 stack_test3)
    (print "SUCCESS")
    (print "FAILURE"))
