(define #apa 10)
(define #bepa 20)
(define #cepa 30)

(setvar '#apa 1)
(setvar '#bepa 2)
(setvar '#cepa 3)

(check (and (=  #bepa 2)
            (= (+ #apa #bepa #cepa) 6)))
