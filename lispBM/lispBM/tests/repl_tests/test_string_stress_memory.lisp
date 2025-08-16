;; String Extensions Memory Stress Tests

(define test-count 0)
(define pass-count 0)

;; Test 1: Large string creation through repeated joining
(define base-str "abcdefghijklmnopqrstuvwxyz")
(define str-list (list base-str base-str base-str base-str))
(define large-str (str-join str-list ""))

(setq test-count (+ test-count 1))
(if (> (str-len large-str) 100)
    (setq pass-count (+ pass-count 1)))

;; Test 2: Multiple string replications
(define sizes (range 1 10))
(define replicated-strings (map (lambda (n) (str-replicate n 65)) sizes))

(setq test-count (+ test-count 1))
(if (= (length replicated-strings) 9)
    (setq pass-count (+ pass-count 1)))

;; Test 3: String replacement operations
(define replace-str (str-replicate 50 65))
(define result-str (str-replace replace-str "A" "B"))

(setq test-count (+ test-count 1))
(if (and (> (str-len result-str) 40) (>= (str-find result-str "B") 0))
    (setq pass-count (+ pass-count 1)))

;; Test 4: String splitting and joining
(define csv-str "a,b,c,d,e,f,g,h,i,j")
(define split-result (str-split csv-str ","))
(define rejoin-result (str-join split-result "-"))

(setq test-count (+ test-count 1))
(if (and (= (length split-result) 10) (> (str-len rejoin-result) 15))
    (setq pass-count (+ pass-count 1)))

;; Test 5: Number to string conversions
(define numbers (range 0 10))
(define number-strings (map str-from-n numbers))
(define combined-numbers (str-join number-strings ""))

(setq test-count (+ test-count 1))
(if (and (= (length number-strings) 10) (> (str-len combined-numbers) 5))
    (setq pass-count (+ pass-count 1)))

;; Final result
(if (= pass-count test-count)
    (print "SUCCESS")
    (print "FAILURE"))