;; These strings may be too long for the reader before getting to exection of the "read" code
;; means this may lead to an untrappable error.
(hide-trapped-error)
(define read_test1_2 (trap (read "(+ 10000000000000000000000000000 (+ 1 2)")))
;(define read_test2_2 (trap (read "(\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" (+ 1 2)")))
(define read_test3_2 (trap (read "(+ 1 (+ 100000000000000000000000000000 2)))")))
(define read_test4_2 (trap (read "(+ 1000000000000000000000000000 [1 2 3")))
(define read_test5_2 (trap (read "(+ 1 [| 1 2 3")))
;(define read_test6_2 (trap (read "(str-len \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)")))
(define read_test7_2 (trap (read "'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''")))
(define read_test8_2 (trap (read "```````````````````````````````````````````````````````````````")))
(define read_test9_2 (trap (read ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,")))
(define read_test10_2 (trap (read ",,@@")))

(print "SUCCESS") ;; no crash = success
