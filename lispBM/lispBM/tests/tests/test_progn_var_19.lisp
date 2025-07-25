(def lst (list 1 2 3 4))

{
(var (a b . rst) lst)
(check (and (= a 1)
            (= b 2)
            (eq rst (list 3 4))))
}
