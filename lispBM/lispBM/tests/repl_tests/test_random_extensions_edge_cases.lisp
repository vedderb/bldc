; Test random extensions edge cases
; seed should take a number argument, test non-numeric inputs
; random should ignore all arguments

; seed with valid numeric arguments (baseline)
(define seed_valid_test1 (eq (seed 42) t))
(define seed_valid_test2 (eq (seed 3.14) t))
(define seed_valid_test3 (eq (seed 0) t))
(define seed_valid_test4 (eq (seed -123) t))

; seed with invalid non-numeric arguments
(define seed_invalid_test1 (eq (trap (seed "string")) '(exit-error eval_error)))
(define seed_invalid_test2 (eq (trap (seed 'symbol)) '(exit-error eval_error)))
(define seed_invalid_test3 (eq (trap (seed '(1 2 3))) '(exit-error eval_error)))
(define seed_invalid_test4 (eq (trap (seed nil)) '(exit-error eval_error)))
(define seed_invalid_test5 (eq (trap (seed t)) '(exit-error eval_error)))
(define seed_invalid_test6 (eq (trap (seed [1 2 3])) '(exit-error eval_error)))

; seed with no arguments
(define seed_no_args_test (eq (trap (seed)) '(exit-error eval_error)))

; seed with too many arguments
(define seed_too_many_test (eq (trap (seed 42 123)) '(exit-error eval_error)))

; random ignores all arguments - should return an unsigned integer regardless
(define random_no_args_test (eq (type-of (random)) 'type-u))
(define random_one_arg_test (eq (type-of (random 42)) 'type-u))
(define random_two_args_test (eq (type-of (random "string" 'symbol)) 'type-u))
(define random_many_args_test (eq (type-of (random 1 2 3 4 5)) 'type-u))
(define random_invalid_args_test (eq (type-of (random nil t '(1 2) [3 4])) 'type-u))

; random should return unsigned 32-bit values  
(define random_range_test1 (let ((r (random))) (and (>= r 0) (<= r 4294967295u32))))
(define random_range_test2 (let ((r (random 999))) (and (>= r 0) (<= r 4294967295u32))))
(define random_range_test3 (let ((r (random "ignore"))) (and (>= r 0) (<= r 4294967295u32))))

; Verify random returns different values (probabilistically)
(define random_different_test1 (not (= (random) (random))))
(define random_different_test2 (not (= (random 1) (random 2))))
(define random_different_test3 (not (= (random "a") (random "b"))))

; Test that seeding affects random output
(define seed_effect_test1 
  (progn
    (seed 12345)
    (define r1 (random))
    (seed 12345)
    (define r2 (random))
    (= r1 r2)))

(define seed_effect_test2
  (progn
    (seed 1)
    (define r3 (random))
    (seed 2) 
    (define r4 (random))
    (not (= r3 r4))))

; Test random with type conversions in seed
(define seed_float_test1 (eq (seed 3.14159) t))
(define seed_float_test2 (eq (seed -2.71828) t))

(if (and seed_valid_test1 seed_valid_test2 seed_valid_test3 seed_valid_test4
         seed_invalid_test1 seed_invalid_test2 seed_invalid_test3 seed_invalid_test4 seed_invalid_test5 seed_invalid_test6
         seed_no_args_test seed_too_many_test
         random_no_args_test random_one_arg_test random_two_args_test random_many_args_test random_invalid_args_test
         random_range_test1 random_range_test2 random_range_test3
         random_different_test1 random_different_test2 random_different_test3
         seed_effect_test1 seed_effect_test2
         seed_float_test1 seed_float_test2)
    (print "SUCCESS")
    (print "FAILURE"))