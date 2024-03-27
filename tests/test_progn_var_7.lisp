
(defun f (x) (cons x (+ x 1)))


(defun g (x)
  {
  (var (a . b) (f x))
  (+ a b)
  }
  )

(check (= (g 3) 7))
