(define apa  0)
(setvar 'apa 1)
(define bepa 2)

(undefine 'apa)

(check (= bepa 2))
