(define #apa 10)
(define #bepa 20)
(define #cepa 30)

(set! '#apa 1)
(set! '#bepa 2)
(set! '#cepa 3)

(and (=  #bepa 2)
     (= (+ #apa #bepa #cepa) 6))
