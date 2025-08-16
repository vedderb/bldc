(define r1 (eq (str-split "This is a test" " ") (list "This" "is" "a" "test")))

(define r2 (eq (str-split "Thisoisoaotest" "o") (list "This" "is" "a" "test")))

(define r3 (eq (str-split "This is a test" 3) (list "Thi" "s i" "s a" " te" "st")))

(define r4 (eq (str-split "This is a test" 1) (list "T" "h" "i" "s" " " "i" "s" " " "a" " " "t" "e" "s" "t")))

(define r5 (eq (str-split "a,b,,c" ",") (list "a" "b" "" "c")))

(define r6 (eq (str-split ",a," ",") (list "" "a" "" )))

(define r7 (eq (str-split "a,b;c.d" ",;.") (list "a" "b" "c" "d")))

(check (and r1 r2 r3 r4 r5 r6 r7))
