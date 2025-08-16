(define s1 "hello")
(define s2 "world")
(define s3 "hello")

(define len-test (str-len s1))
(define cmp-test1 (str-cmp s1 s2))
(define cmp-test2 (str-cmp s1 s3))

(if (and (= len-test 5)
         (< cmp-test1 0)
         (= cmp-test2 0))
    (print "SUCCESS")
  (print "FAILURE"))