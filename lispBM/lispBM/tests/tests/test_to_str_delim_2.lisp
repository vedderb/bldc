(define r1 (eq (to-str-delim "::" "aAa" 4 '(a 2 3) 2 3 "Hello") "aAa::4::(a 2 3)::2::3::Hello"))

(check r1)
