(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(define feq (lambda (a b epsilon)
              (< (abs (- a b)) epsilon)))

@const-start

(define pi_half 1.5708)         ; π/2 in radians
(define pi_quarter 0.7854)      ; π/4 in radians  
(define deg_90 90.0)            ; 90 degrees
(define deg_45 45.0)            ; 45 degrees
(define deg_180 180.0)          ; 180 degrees

(define e_const 2.7183)         ; Euler's number (approximate)
(define sqrt_arg 16.0)          ; Perfect square for sqrt
(define pow_base 2.0)           ; Base for power operations
(define pow_exp 3.0)            ; Exponent for power operations
(define log_arg 10.0)           ; Argument for logarithm
(define neg_val -5.5)           ; Negative value
(define large_val 100.5)        ; Large positive value

(define zero_val 0.0)
(define one_val 1.0)
(define nan_input 2.0)          ; Will be used to create NaN via sqrt(-x)
(define inf_input 1000000.0)    ; Large value for potential infinity

(define thousand 1000u32)

(define nan_val (sqrt neg_val))
(define inf_val (exp 1000))

(define nan_double (to-double nan_val))
(define inf_double (to-double inf_val))

@const-end

;; Test 1
(define trig-tests
  (and
    ; Basic trigonometric functions
    (feq (sin pi_half) 1.0 0.001)           ; sin(π/2) = 1
    (feq (cos zero_val) 1.0 0.001)          ; cos(0) = 1
    (feq (tan pi_quarter) 1.0 0.001)        ; tan(π/4) = 1
    
    ; Inverse trigonometric functions
    (feq (asin one_val) pi_half 0.001)      ; asin(1) = π/2
    (feq (acos zero_val) pi_half 0.001)     ; acos(0) = π/2
    (feq (atan one_val) pi_quarter 0.001)   ; atan(1) = π/4
    
    ; Two-argument arctangent
    (feq (atan2 one_val one_val) pi_quarter 0.001)  ; atan2(1,1) = π/4
    (feq (atan2 one_val zero_val) pi_half 0.001)    ; atan2(1,0) = π/2
  ))

;; Test 2
(define exp-log-tests
  (and
    ; Exponential functions
    (feq (exp zero_val) 1.0 0.001)          ; e^0 = 1
    (feq (exp one_val) e_const 0.01)        ; e^1 ≈ e
    
    ; Power function
    (feq (pow pow_base pow_exp) 8.0 0.001)  ; 2^3 = 8
    (feq (pow sqrt_arg 0.5) 4.0 0.001)      ; 16^0.5 = 4
    
    ; Square root
    (feq (sqrt sqrt_arg) 4.0 0.001)         ; √16 = 4
    (feq (sqrt one_val) 1.0 0.001)          ; √1 = 1
    
    ; Logarithmic functions
    (feq (log e_const) 1.0 0.01)            ; ln(e) = 1
    (feq (log10 log_arg) 1.0 0.001)         ; log₁₀(10) = 1
  ))

;; Test 3
(define round-tests
  (and
    ; Floor function
    (feq (floor large_val) 100.0 0.001)     ; floor(100.5) = 100
    (feq (floor neg_val) -6.0 0.001)        ; floor(-5.5) = -6
    
    ; Ceiling function
    (feq (ceil large_val) 101.0 0.001)      ; ceil(100.5) = 101
    (feq (ceil neg_val) -5.0 0.001)         ; ceil(-5.5) = -5
    
    ; Round function
    (feq (round large_val) 101.0 0.001)     ; round(100.5) = 101
    (feq (round neg_val) -6.0 0.001)        ; round(-5.5) = -6
  ))

;; Test 4
(define angle-conv-tests
  (and
    ; Degree to radian conversion
    (feq (deg2rad deg_90) pi_half 0.001)    ; 90° = π/2 rad
    (feq (deg2rad deg_45) pi_quarter 0.001) ; 45° = π/4 rad
    (feq (deg2rad deg_180) 3.1416 0.001)    ; 180° = π rad
    
    ; Radian to degree conversion
    (feq (rad2deg pi_half) deg_90 0.001)    ; π/2 rad = 90°
    (feq (rad2deg pi_quarter) deg_45 0.001) ; π/4 rad = 45°
    
    ; Multiple argument conversions
    (let ((deg_list (deg2rad deg_45 deg_90))
          (rad_list (rad2deg pi_quarter pi_half)))
      (and (= (length deg_list) 2)
           (= (length rad_list) 2)
           (feq (car deg_list) pi_quarter 0.001)
           (feq (car rad_list) deg_45 0.001)))
  ))

;; Test 5
(define special-tests
  (and
    ; Test is-nan function
    (not (is-nan one_val))                   ; 1.0 is not NaN
    (not (is-nan zero_val))                  ; 0.0 is not NaN
    (not (is-nan neg_val))                   ; -5.5 is not NaN
    
    ; Test is-inf function  
    (not (is-inf one_val))                   ; 1.0 is not infinite
    (not (is-inf zero_val))                  ; 0.0 is not infinite
    (not (is-inf large_val))                 ; 100.5 is not infinite
  ))

;; Test 6
(define error-tests
  (and
    ; Domain errors should be handled gracefully
    (is-nan (sqrt neg_val))                  ; √(-5.5) should be NaN
    (is-nan (asin large_val))                ; asin(100.5) should be NaN (domain error)
    (is-nan (acos large_val))                ; acos(100.5) should be NaN (domain error)
    (is-nan (log neg_val))                   ; ln(-5.5) should be NaN (domain error)
    
    (is-inf (exp thousand))  ; overflows into infinity

    (is-nan nan_val)
    (is-inf inf_val)

    (is-nan nan_double)
    (is-inf inf_double)
    
  ))

; Debug each test group
(debug_test trig-tests 1)
(debug_test exp-log-tests 2)
(debug_test round-tests 3)
(debug_test angle-conv-tests 4)
(debug_test special-tests 5)
(debug_test error-tests 6)

; Run all tests
(if (and trig-tests exp-log-tests round-tests angle-conv-tests special-tests error-tests)
    (print "SUCCESS")
    (print "FAILURE"))
