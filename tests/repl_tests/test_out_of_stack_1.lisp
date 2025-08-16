

;; Runs until stack is full
(defun f (x)
  (+ 1 (f (+ x 1))))


(define r (trap (f 0)))

(if (eq r '(exit-error out_of_stack))
    (print "SUCCESS")
    (print "FAILURE")
    )
