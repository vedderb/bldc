;; String Extensions Edge Case Stress Tests

(define test-count 0)
(define pass-count 0)

;; Test 1: Empty string operations
(define empty-str "")
(define empty-len (str-len empty-str))
(define empty-upper (str-to-upper empty-str))

(setq test-count (+ test-count 1))
(if (and (= empty-len 0) (= (str-len empty-upper) 0))
    (setq pass-count (+ pass-count 1)))

;; Test 2: Single character operations  
(define single-char "A")
(define single-len (str-len single-char))
(define single-lower (str-to-lower single-char))

(setq test-count (+ test-count 1))
(if (and (= single-len 1) (= (str-cmp single-lower "a") 0))
    (setq pass-count (+ pass-count 1)))

;; Test 3: Large string operations
(define long-str (str-replicate 500 65))
(define long-len (str-len long-str))

(setq test-count (+ test-count 1))
(if (= long-len 500)
    (setq pass-count (+ pass-count 1)))

;; Test 4: String splitting edge cases
(define split-no-delim (str-split "abc" ","))
(define split-normal (str-split "a,b,c" ","))

(setq test-count (+ test-count 1))
(if (and (= (length split-no-delim) 1) (= (length split-normal) 3))
    (setq pass-count (+ pass-count 1)))

;; Test 5: String replacement edge cases
(define repl-not-found (str-replace "abc" "xyz" "123"))
(define repl-normal (str-replace "abc" "b" "X"))

(setq test-count (+ test-count 1))
(if (and (= (str-cmp repl-not-found "abc") 0) (= (str-cmp repl-normal "aXc") 0))
    (setq pass-count (+ pass-count 1)))

;; Test 6: String find edge cases
(define find-not-found (str-find "abc" "xyz"))
(define find-normal (str-find "abcdef" "def"))

(setq test-count (+ test-count 1))
(if (and (= find-not-found -1) (= find-normal 3))
    (setq pass-count (+ pass-count 1)))

;; Test 7: Number conversion edge cases
(define zero-str (str-from-n 0))
(define neg-str (str-from-n -42))

(setq test-count (+ test-count 1))
(if (and (= (str-cmp zero-str "0") 0) (= (str-to-i neg-str) -42))
    (setq pass-count (+ pass-count 1)))

;; Test 8: String part edge cases
(define test-str "abcdef")
(define part-first (str-part test-str 0 1))
(define part-last (str-part test-str 5 1))

(setq test-count (+ test-count 1))
(if (and (= (str-cmp part-first "a") 0) (= (str-cmp part-last "f") 0))
    (setq pass-count (+ pass-count 1)))

;; Final result
(if (= pass-count test-count)
    (print "SUCCESS")
    (print "FAILURE"))