
@const-start

(define a 3.14)
(define b 6.28f64)

(define c 2)

@const-end


(if (and (eq (str-from-n a "%.1f") "3.1")
         (eq (str-from-n b "%.2f") "6.28")
         (eq (str-from-n c) "2"))
    (print "SUCCESS")
    (print "FAILURE"))
