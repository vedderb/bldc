(check (let ((r 100)
             (a 250))
         (and ( = r (eval-program (read-program "(+ 50 50)")))
              ( = a (eval-program (read-program "(+ 20 20) (+ 100 150)"))))))
