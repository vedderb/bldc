
(defun tailfun (x)
  (+ x 1000))

(defun f (x z)
  ( match x
          ( (apa (? y)) { (+ y z) (gc) (tailfun y) } )
          ( (bepa (? y)) { (+ y z) (gc) (tailfun z) } )
          ))

(check (and ( = (f '(apa 10) 20) 1010)
            ( = (f '(bepa 10) 20) 1020)))
