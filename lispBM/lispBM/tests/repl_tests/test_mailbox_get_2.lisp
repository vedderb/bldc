

(define a (mailbox-get 1))
(define b (trap (mailbox-get)))
(define c (trap (mailbox-get 'apa)))

(if (and (eq a nil)
         (eq b '(exit-error type_error))
         (eq c '(exit-error type_error)))
    (print "SUCCESS")
  (print "FAILURE"))
