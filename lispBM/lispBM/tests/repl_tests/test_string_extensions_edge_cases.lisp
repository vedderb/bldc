;; Additional tests for string extensions to increase branch coverage

(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

;; Test str-part with start index >= string length (should return eval_error)
(define r1 (eq (ix (trap (str-part "hello" 10)) 0) 'exit-error))
(debug_test r1 1)

;; Test str-part with start index at exactly string length
(define r2 (eq (ix (trap (str-part "hello" 5)) 0) 'exit-error))
(debug_test r2 2)

;; Test str-find with zero-length substring in array form
(define r3 (= (str-find "hello" []) -1))
(debug_test r3 3)

;; Test str-find with zero-length substring in list form 
(define r4 (= (str-find "hello" '([])) -1))
(debug_test r4 4)

;; Test str-find with case-insensitive search
(define r5 (= (str-find "Hello World" "WORLD" 'nocase) 6))
(debug_test r5 5)

;; Test str-find with left direction search
(define r6 (= (str-find "hello hello" "hello" 'left) 6))
(debug_test r6 6)

;; Test str-find with negative start index
(define r7 (= (str-find "hello" "l" -2) 3))
(debug_test r7 7)

;; Test str-find with occurrence parameter
(define r8 (= (str-find "hello hello" "l" 0 1) 3))
(debug_test r8 8)

;; Test str-find with both start and occurrence parameters
;; Indexing from occurrence 0
(define r9 (= (str-find "hello hello hello" "hello" 1 1) 12))
(debug_test r9 9)

;; Test str-find with case-insensitive and left direction
(define r10 (= (str-find "Hello HELLO" "hello" 'nocase 'left) 6))
(debug_test r10 10)

;; Test str-find with list of substrings
(define r11 (= (str-find "hello world" '("foo" "world")) 6))
(debug_test r11 11)

;; Test str-find where substring runs over string end
(define r12 (= (str-find "hi" "hello") -1))
(debug_test r12 12)

;; Test str-replicate with character value (second param as number)
(define r13 (eq (str-replicate 5 65) "AAAAA"))
(debug_test r13 13)

;; Test str-replicate with zero length
(define r14 (eq (str-replicate 0 65) ""))
(debug_test r14 14)

;; Test str-cmp with length limit
(define r15 (= (str-cmp "Hello" "Helloworld" 5) 0))
(debug_test r15 15)

;; Test str-find with start position that gets adjusted for left search
(define r16 (= (str-find "hello" "e" 10 'left) 1))
(debug_test r16 16)

;; Test str-find with start position that gets adjusted for right search  
(define r17 (= (str-find "hello" "e" -10) 1))
(debug_test r17 17)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17)
    (print "SUCCESS")
    (print "FAILURE"))
