


(send (self) '(apa 129))

(= (recv
    ( (bepa (? x)) 'bepa)
    ( (apa  (? x)) x))
   129)
