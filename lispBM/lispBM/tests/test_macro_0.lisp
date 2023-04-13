(define defun (macro (name args body)
                     `(define ,name (lambda ,args ,body))))

(defun f (x y) (+ x y))

(check (= (f 1 2) 3))
