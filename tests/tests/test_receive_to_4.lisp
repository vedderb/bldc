


(define r (recv-to 0.1
                (timeout 'no-message)
                (_ 1)))

(check (eq r 'no-message))
