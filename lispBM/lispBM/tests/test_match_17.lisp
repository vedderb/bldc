

(defun f (x y z)
  ( match x
          ( apa { (+ y z) (gc) y } )
          ( bepa { (+ y z) (gc) z } )
          ))

(check (and ( = (f 'apa 10 20) 10)
            ( = (f 'bepa 10 20) 20)))
