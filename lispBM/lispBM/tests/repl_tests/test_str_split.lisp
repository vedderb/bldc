

;; Test for str-split functionality improvement

(define r1 (str-split "a,b,c" ","))
(define r2 (str-split "apa,bepa,,cepa,depa" ","))
(define r3 (str-split "a,b,,c," ","))
(define r4 (str-split ",apa,bepa,,cepa,depa" ","))

(if (and (eq r1 (list "a" "b" "c"))
         (eq r2 (list "apa" "bepa" "" "cepa" "depa"))
         (eq r3 (list "a" "b" "" "c" ""))
         (eq r4 (list "" "apa" "bepa" "" "cepa" "depa")))
    (print "SUCCESS")
    (print "FAILURE"))
         
             
