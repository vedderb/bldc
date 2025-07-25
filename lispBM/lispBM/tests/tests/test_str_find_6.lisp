(define r1 (= (str-find "-ab-" '("")) 0))

(define r2 (= (str-find "-ab-" '("") 'left) 5))


(check (and r1 r2))
