

(defun f ()
  (let (( a 0 )
        ( b 2 )
        ( c 3 ))
    (progn
      (setq a 100)
      (setq b 200)
      (setq c 300)
      (+ a b c))))


(check (= (f) 600))
