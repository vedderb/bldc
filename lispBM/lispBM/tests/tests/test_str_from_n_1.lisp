
(gc)

(define n (mem-num-free))

(str-from-n 132)

(gc)

(check (= n (mem-num-free)))
