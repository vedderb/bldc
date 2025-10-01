(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(define r1 (eq (str-replace "hej" "" "ay") "hej"))
(debug_test r1 1)

(define r2 (eq (str-replace "hej" "ej" "") "h"))
(debug_test r2 2)

(define r3 (eq (str-replace "hej" "" "") "hej"))
(debug_test r3 3)

(define r4 (eq (str-replace "hej" "ej") "h"))
(debug_test r4 4)

(define r5 (eq '(exit-error type_error) (trap (str-replace "hej" "ej" 1))))
(debug_test r5 5)

(define r6 (eq '(exit-error type_error) (trap (str-replace "hej" 1 1))))
(debug_test r6 6)

(define r7 (eq '(exit-error type_error) (trap (str-replace "hej" 1))))
(debug_test r7 7)

(define r8 (eq '(exit-error eval_error) (trap (str-replace 1))))
(debug_test r8 8)

(define r9 (eq '(exit-error eval_error) (trap (str-replace))))
(debug_test r9 9)

(if (and r1 r2 r3
         r4 r5 r6
         r7 r8 r9)
    (print "SUCCESS")
    (print "FAILURE"))
