(defun f (x z)
  ( match x
          ( (apa (? y)) { (+ y z) (gc) y } )
          ( (bepa (? y)) { (+ y z) (gc) z } )
          ))

(check (and ( = (f '(apa 10) 20) 10)
            ( = (f '(bepa 10) 20) 20)))
