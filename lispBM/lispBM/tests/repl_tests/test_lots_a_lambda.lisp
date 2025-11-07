
(define a (lambda (x) (+ x 1 )))


;;  looprange or other loop macros that
;;  creates a local environment holding the body lead to
;;  expression size explosion.
(defun f (n)
  (if (= n 0) ()
      {
      ( setq a (lambda (x) (+ x n)))
      (f (- n 1))
      }))

(f 400000)

(if (= (a 1) 2)
    (print "SUCCESS")
    (print "FAILURE"))
