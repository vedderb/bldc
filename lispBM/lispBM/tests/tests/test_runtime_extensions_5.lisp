
(define apa 1)


(define find-env (lambda (i n1)
  (if (not (eq (env-get i) nil))
      i
    (find-env (+ i 1) n1))))

(define e-ix (find-env 0 100))

(define e (env-get e-ix))

(env-set e-ix nil)

(define r1 (eq '(exit-error variable_not_bound) (trap apa)))

(env-set e-ix e)

(define r2 (= 1 apa))

(check (and r1 r2))
