

;; (a 10) is similar to a global but only global to the two functions
;; created in the let.
;; This may be a bit strange. But strangeness only observable through
;; imperative updates.
(define funs (let ( ( a 10) )
               (list (lambda (x) { (setq a x) a})
                     (lambda () a))))

(define f (car funs))

(define g (car (cdr funs)))

(f 20)

(check (= (g) 20))
