


(event-register-handler (self))


(spawn (fn ()
           (event-sym 'apa)))



(check (recv ((? x) (eq x 'apa))))
