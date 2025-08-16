; Test integer division (//) with floating-point arguments
; Ensure proper conversion from f32/f64 to integer results

; Integer division with f32 arguments
(define idiv_f32_test1 (eq (// 10.5f32 2.0f32) 5))
(define idiv_f32_test2 (eq (// 15.9f32 3.2f32) 4))
(define idiv_f32_test3 (eq (// 7.1f32 2.9f32) 2))
(define idiv_f32_test4 (eq (// 20.0f32 4.0f32) 5))
(define idiv_f32_test5 (eq (// -10.5f32 2.0f32) -5))
(define idiv_f32_test6 (eq (// 9.9f32 -3.1f32) -3))

; Integer division with f64 arguments  
(define idiv_f64_test1 (eq (// 10.5 2.0) 5))
(define idiv_f64_test2 (eq (// 15.9 3.2) 4))
(define idiv_f64_test3 (eq (// 7.1 2.9) 2))
(define idiv_f64_test4 (eq (// 20.0 4.0) 5))
(define idiv_f64_test5 (eq (// -10.5 2.0) -5))
(define idiv_f64_test6 (eq (// 9.9 -3.1) -3))

; Mixed f32/f64 arguments
(define idiv_mixed_test1 (eq (// 10.5f32 2.0) 5))
(define idiv_mixed_test2 (eq (// 15.9 3.2f32) 4))
(define idiv_mixed_test3 (eq (// -7.1f32 2.9) -2))
(define idiv_mixed_test4 (eq (// 8.8 -2.2f32) -4))

; Mixed integer/float arguments
(define idiv_int_float_test1 (eq (// 10 2.5f32) 4))
(define idiv_int_float_test2 (eq (// 15.5f32 3) 5))
(define idiv_int_float_test3 (eq (// 12 3.0) 4))
(define idiv_int_float_test4 (eq (// 14.0 7) 2))

; Edge cases with small values
(define idiv_small_test1 (eq (// 0.9f32 0.1f32) 9))
(define idiv_small_test2 (eq (// 0.99 0.33) 3))
(define idiv_small_test3 (eq (// 1.1f32 0.9f32) 1))

; Verify result type is integer
(define result_type_test1 (eq (type-of (// 10.5f32 2.0f32)) 'type-i))
(define result_type_test2 (eq (type-of (// 15.9 3.2)) 'type-i))
(define result_type_test3 (eq (type-of (// 10.5f32 2.0)) 'type-i))

; Division by zero should still error
(define div_zero_test1 (eq (trap (// 10.5f32 0.0f32)) '(exit-error division_by_zero)))
(define div_zero_test2 (eq (trap (// 15.9 0.0)) '(exit-error division_by_zero)))

(if (and idiv_f32_test1 idiv_f32_test2 idiv_f32_test3 idiv_f32_test4 idiv_f32_test5 idiv_f32_test6
         idiv_f64_test1 idiv_f64_test2 idiv_f64_test3 idiv_f64_test4 idiv_f64_test5 idiv_f64_test6
         idiv_mixed_test1 idiv_mixed_test2 idiv_mixed_test3 idiv_mixed_test4
         idiv_int_float_test1 idiv_int_float_test2 idiv_int_float_test3 idiv_int_float_test4
         idiv_small_test1 idiv_small_test2 idiv_small_test3
         result_type_test1 result_type_test2 result_type_test3
         div_zero_test1 div_zero_test2)
    (print "SUCCESS")
    (print "FAILURE"))