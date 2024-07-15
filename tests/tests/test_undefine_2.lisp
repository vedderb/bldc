
(define apa 1)
(define bepa 0)
(setvar 'bepa 2)

(undefine 'apa)

(check (= bepa 2))
