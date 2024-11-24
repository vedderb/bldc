(define r1 (= (str-cmp "Hello" "Hello") 0))
(define r2 (< (str-cmp "Hello" "World") 0))
(define r3 (> (str-cmp "World" "Hello") 0))
(define r4 (< (str-cmp "ab" "abcd") 0))

(check (and r1 r2 r3 r4))
