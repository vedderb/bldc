
(define apa 1)
(setvar 'bepa 2)

(undefine 'apa)

(check (= bepa 2))
