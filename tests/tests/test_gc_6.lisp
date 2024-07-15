
(define tree '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8 ))))
(define tree2 (cons tree tree))

(gc)

(check (and
        (eq tree '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8 ))))
        (eq (car tree2) '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8 ))))
        (eq (cdr tree2) '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8 ))))))
       
