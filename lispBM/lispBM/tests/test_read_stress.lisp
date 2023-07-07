
(defun repeat_eval (c n)
  (if ( = n 0)
      ()
    (progn
      (c n)
      (bufcreate 10)
      (repeat_eval c (- n 1)))))



(defun code (x) (def apa (eval `(read (str-merge "bepa" (str-from-n x))))))

; Create just enough symbols and symbols and arrays to trigger GC.
(repeat_eval code 432)

(check (eq apa 'bepa1))
