

(def r (atomic
        (var a 10)
        (var b 20)
        (+ a b)))

(check (= r 30))
