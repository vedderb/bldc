
(check (and
        (eq (read (to-str 'hello)) 'hello)
        (eq (read (to-str '(+ 1 2))) '(+ 1 2))
        (eq (read (to-str '(1 2 3 (4 5 6)))) '(1 2 3 (4 5 6)))))

