
(define a '( (1 . 'apa) (3 . 'cepa)))
(define b '( (2 . 'kurt) (4 . 'russel)))
     

(define cmp
  (lambda (a b) (< (car a) (car b))))
  
(check (eq (merge cmp a b) '( (1 . 'apa)  (2 . 'kurt) (3 . 'cepa) (4 . 'russel))))
          
