(define r1 (= (str-find "-str-" "str") 1))

(define r2 (= (str-find "-str-str-" "str" 0 1) 5))

(define r3 (= (str-find "-str-str-" "str" 2) 5))

(define r4 (= (str-find "-str-str-" "str" 'left) 5))

(define r5 (= (str-find "-ab-ab-" "ab" 5 'left) 4))

(define r6 (= (str-find "-ab-ba-" '("ba" "ab") 0 1) 4))

(define r7 (= (str-find "a--a" "a" -1 'left) 3))

(check (and r1 r2 r3 r4 r5 r6 r7))
