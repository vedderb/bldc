


{
(var (a b . cs) (list 1 2 3 4 5 6 7 8))

(check (and (= a 1)
            (= b 2)
            (eq cs (list 3 4 5 6 7 8))))
}
