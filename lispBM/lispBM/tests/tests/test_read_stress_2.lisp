
(defun apply (f args)
  (eval (cons f args)))


(defun f ()
  (apply + (rest-args)))


(defun repeat_eval (c res n)
  (if ( = n 0)
      t
    (if (!= (c) res) nil
      (repeat_eval c res(- n 1)))))

(define str "(f 1 2 3 4 5 6 7 8 9 10)")

(defun code () (eval (read str)))



(check (repeat_eval code 55 10000))


