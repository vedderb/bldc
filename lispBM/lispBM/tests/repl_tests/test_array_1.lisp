
(define a (array))
(define b (array 1 2 3))
(define c (array (+ 1 2) (+ 2 3)))

(if (and (eq a [| |])
         (eq b [| 1 2 3 |])
         (eq c [| 3 5 |]))
    (print "SUCCESS")
  (print "FAILURE")
  )
