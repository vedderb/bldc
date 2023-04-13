
(defun f (x)
  (match x
         ( apa 1 )
         ( bepa 2 )))

(move-to-flash f)

(check (and (= (f 'apa) 1)
            (= (f 'bepa) 2)))
