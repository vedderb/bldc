


(event-register-handler (self))


(spawn (fn ()
           (event-sym 'apa)))



(recv ((? x) (eq x 'apa)))
