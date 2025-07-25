
(define a (mkarray 10))

(loopfor i 0 (< i 10000) (+ i 1)
         {
         (setq a (mkarray 10))
         (setix a 0 10)
         (setix a 1 'apa)
         }
         )

(check (and (= (ix a 0) 10)
            (eq (ix a 1) 'apa)))
                
