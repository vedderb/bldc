(define a 10)

(defun f (n)
  (if (= n 0) 0
    (progn
      (define a 10)
      (define b 100)
      (undefine 'b)
      (f (- n 1)))))

(f 10000)
(undefine 'a)
(define c 5)
(f 10000)

(define b 75)

(check (and (= b 75)
            (= c 5)))
