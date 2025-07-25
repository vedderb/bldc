(def a 'hello)

(let
    ((a a))
    (check (eq a 'hello))
)