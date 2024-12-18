

(define r1 (recv-to 0.1 (apa 10)))


(check (eq r1 'timeout))
