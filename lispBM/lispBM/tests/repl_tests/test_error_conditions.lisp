(define result1 
  (trap (eval 'undefined-function)))

(define result2
  (trap (car 42)))

(define result3
  (trap (/ 1 0)))

(if (and (eq (car result1) 'exit-error)
         (eq (car result2) 'exit-error)
         (eq (car result3) 'exit-error))
    (print "SUCCESS")
  (print "FAILURE"))