
(define s "hello\t\r\n\\")

(define r (print s))

(if r (print "SUCCESS")
    (print "FAILURE"))
