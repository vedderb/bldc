
(def prg "(define a 10) (+ a 100)")


(check (= (read-eval-program prg) 110))
