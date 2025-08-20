
(define a (string? "hello"))
(define b (string? 'apa))
(define c (trap (string?)))

(if (and (eq a 't)
         (eq b nil)
         (eq c '(exit-error type_error)))
    (print "SUCCESS")
  (print "FAILURE")
  )
