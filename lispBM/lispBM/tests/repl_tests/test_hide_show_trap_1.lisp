
(define a (hide-trapped-error))
(define b (show-trapped-error))

(if (and (eq a 't)
         (eq b 't))
    (print "SUCCESS")
  (print "FAILURE"))
