

(define a1 (if)) ;; nil 

(define a2 (if [ 1 2 3])) ;; Should be nil

(define a3 (trap (if . [1 2 3]))) ;; Should be error 

(define a4 (trap (if t . [1 2 3]))) ;; Should be error


(if (and (eq a1 nil)
         (eq a2 nil)
         (eq a3 '(exit-error type_error))
         (eq a4 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE")
    )
