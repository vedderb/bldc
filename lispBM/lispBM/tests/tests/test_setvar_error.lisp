
(define eerr '(exit-error eval_error))
(define verr '(exit-error variable_not_bound))

(define r1 (eq eerr (trap (setvar 2 1))))
(define r2 (eq eerr (trap (setvar 'apa 1 2))))
               
(define r3 (eq eerr (trap (set 2 1))))
(define r4 (eq eerr (trap (set 'apa 1 2))))

(define r3 (eq eerr (trap (setq 2 1))))
(define r4 (eq verr (trap (setq apa 1 2))))

(define apa 0)

(define r5 (eq 1 (setq apa 1 2)))

;; The set operations are very similar.
;; setq behaves a differently when given the incorrect
;; number or type of arguments. 
(check (and r1 r2 r3 r4 r5))

