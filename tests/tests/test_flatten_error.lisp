

(define data (list (list (list 1 2 3))))

; should be file
(define r1 (eq (unflatten (flatten data)) data))

(flatten-depth 2)

;should fail
(define r2 (eq '(exit-error eval_error) (trap (unflatten (flatten data)))))


(check (and r1 r2))


