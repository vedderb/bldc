

(defun f (x)
  (lambda (y) (rest-args)))


(define g (f 1 2 3 4 5))

(check (and (eq (g 1) '(2 3 4 5))
            (eq (g 1 2) '(2))))
