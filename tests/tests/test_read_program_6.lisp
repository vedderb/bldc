(check (let ((r (eval-program (read-program "(+ 50 50)")))
             (a (eval-program (read-program "(+ 20 20) (+ 100 150)"))))
         (and ( = r 100)
              ( = a 250))))
