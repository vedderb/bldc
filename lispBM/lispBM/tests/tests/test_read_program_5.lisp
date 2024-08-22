(check (let ((r (eval-program (read-program "(+ 50 50)"))))
         (= r 100)))
