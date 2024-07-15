

(event-register-handler (self))

(defun event-sender (n)
  (if (= 0 n) ()
    (progn
      (event-sym 'apa)
      (iota 271) ; Trigger GC eventually
      (event-sender (- n 1)))))
    
(spawn event-sender 100)



(check (and (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))
            (recv ((? x) (eq x 'apa)))))
     

