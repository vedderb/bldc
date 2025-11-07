
(define r1 (trap (send))) ;; eerror
(define r2 (trap (send [| 1 2 3 |] 'apa))) ;; terror


(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error type_error)))
         (print "SUCCESS")
         (print "FAILURE")
         )
    
        
