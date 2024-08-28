(define r1 (eq (to-str '(1 2 3)) "(1 2 3)"))

(define r2 (eq (to-str "aAa" 4 '(a 2 3) 2 3 "Hello") "aAa 4 (a 2 3) 2 3 Hello"))

(check (and r1 r2))
