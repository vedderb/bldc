(define sym1 (str2sym "test-sym"))
(define sym2 'test-sym)
(define sym3 (str2sym "another-sym"))

(define str1 (sym2str sym1))
(define str2 (sym2str sym2))

(if (and (eq sym1 sym2)
         (not (eq sym1 sym3))
         (str-cmp str1 "test-sym")
         (= (str-cmp str1 str2) 0))
    (print "SUCCESS")
  (print "FAILURE"))
