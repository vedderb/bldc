
(def a (const-prg))

(check (= (eval-program (read-program (const-prg))) 11))
