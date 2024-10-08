
(defun apply (f args)
  (eval (cons f args)))

(defun test-it (n c res)
  (if (= n 0) t
    (progn
      (var args (range 1 11))
      (if (!= (apply c args) res) nil
        (test-it (- n 1) c res)))))

(defun f-rest-args ()
  (apply + (rest-args)))

(def res (test-it 10000 f-rest-args 55))

(check res)

