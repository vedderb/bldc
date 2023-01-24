
(event-register-handler (self))


(spawn (fn ()
           (event-array 'apa)))



(recv (((? x) . (? arr)) (and (eq x 'apa) (eq arr "Hello world"))))
