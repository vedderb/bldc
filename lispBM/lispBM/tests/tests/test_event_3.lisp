(event-register-handler (self))


(spawn (fn ()
           (progn 
             (event-sym 'apa)
             (event-sym 'bepa)
             (event-array 'cepa))))



(recv ( apa  1))
(recv ( bepa 2))
(recv (( cepa . (? arr)) 3))



(check 't)
