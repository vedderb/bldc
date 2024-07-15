
(def a (const-prg))

(check (= (eval (read (const-prg))) 10))
