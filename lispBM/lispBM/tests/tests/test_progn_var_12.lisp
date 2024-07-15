

(define i1
  (let ( (apa 5)
         (bepa (progn (var apa 10)
                      (setq apa 7)
                      apa)))
    apa))


(check (= i1 5))
