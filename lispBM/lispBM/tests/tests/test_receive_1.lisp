


(send (self) '(apa 129))

(check (= (recv
           ( (bepa (? x)) 'bepa)
           ( (apa  (? x)) x))
          129))
