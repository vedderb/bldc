
(event-register-handler (self))


(spawn (fn ()
           (event-array 'apa)))



(check (recv (((? x) . (? arr)) (and (eq x 'apa) (eq arr "hello world")))))
