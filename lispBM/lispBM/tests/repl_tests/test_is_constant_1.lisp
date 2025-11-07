
@const-start
(define l (list 1 2 3))
@const-end

(define a (constant? l))
(define b (constant? (list 1 2 3)))
(define c (trap (constant?)))

(if (and (eq a 't)
         (eq b nil)
         (eq c '(exit-error type_error)))
    (print "SUCCESS")
  (print "FAILURE"))
         
