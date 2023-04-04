

(check (let ((r 100))
         (= r (eval-program (read-program "(+ 50 50)")))))
