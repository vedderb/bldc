
(defun repeat (f n)
  (if ( = n 0)
      ()
    (progn
      (f n)
      (repeat f (- n 1)))))
                    

(gc)

(define n (mem-num-free))

(repeat (lambda (n) (str-from-n n)) 1000)

(gc)

(check (= n (mem-num-free)))
