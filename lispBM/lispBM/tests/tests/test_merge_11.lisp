
(define a '(apa bepa))
(define b '(apa bepa bepa))

(define cmp
  (lambda (a b)
    (match (cons a b)
           ( (apa . apa) t)
           ( (apa . bepa) t)
           ( (bepa . apa) nil)
           ( (bepa . bepa) t))))

(check (eq (merge cmp a b) '(apa apa bepa bepa bepa)))
          
