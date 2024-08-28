
;; the me-loopfor is only called from a macro loopfor.
;; When using the macro loopfor the eval_error never reaches the user
;; as the macro always instantiates me-loopfor with the right number of args.
(define r1 (trap (me-loopfor)))
(define r2 (trap (me-loopfor 1)))
(define r3 (trap (me-loopfor 'a 1)))
(define r4 (trap (me-loopfor 'i 'j 'k 1)))
(define r5 (trap (me-loopfor '1 2 3 4 5 6)))

(defun testit (x) (eq '(exit-error eval_error) x))

(check (and (testit r1)
            (testit r2)
            (testit r3)
            (testit r4)
            (testit r5)))
            
