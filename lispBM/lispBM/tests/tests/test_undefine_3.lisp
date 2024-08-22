
(define a 10)

(defun f (n)
  (if (= n 0) 0
    (progn
      (define b 100)
      (undefine 'b)
      (f (- n 1)))))

(f 10000)

(define b 75)

(check (and (= a 10)
            (= b 75)))


      
