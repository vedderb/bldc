;; String Extensions Performance Stress Tests

(define test-count 0)
(define pass-count 0)

;; Test 1: String concatenation performance
(define start1 (systime))
(define numbers (range 0 50))
(define number-strings (map str-from-n numbers))
(define concat-str (str-join number-strings ""))
(define end1 (systime))

(setq test-count (+ test-count 1))
(if (> (str-len concat-str) 0)
    (setq pass-count (+ pass-count 1)))

;; Test 2: String comparison performance
(define start2 (systime))
(define str1 "test-string-abc")
(define str2 "test-string-abc")
(define str3 "test-string-xyz")
(define comparisons (list 
    (str-cmp str1 str2)
    (str-cmp str1 str3)
    (str-cmp str2 str1)))
(define end2 (systime))

(setq test-count (+ test-count 1))
(if (= (first comparisons) 0)
    (setq pass-count (+ pass-count 1)))

;; Test 3: String search performance  
(define start3 (systime))
(define search-str (str-replicate 20 65))
(setq search-str (str-join (list search-str "TARGET")))
(define search-results (map (lambda (x) (str-find search-str "TARGET")) (range 0 20)))
(define end3 (systime))

(setq test-count (+ test-count 1))
(if (= (first search-results) 20)
    (setq pass-count (+ pass-count 1)))

;; Test 4: Case conversion performance
(define start4 (systime))
(define test-words (list "Hello" "World" "Test" "String"))
(define upper-words (map str-to-upper test-words))
(define lower-words (map str-to-lower upper-words))
(define end4 (systime))

(setq test-count (+ test-count 1))
(if (and (= (length upper-words) 4) (= (length lower-words) 4))
    (setq pass-count (+ pass-count 1)))

;; Test 5: String splitting performance
(define start5 (systime))
(define csv-data (list "a,b,c" "d,e,f" "g,h,i"))
(define split-results (map (lambda (s) (str-split s ",")) csv-data))
(define end5 (systime))

(setq test-count (+ test-count 1))
(if (= (length split-results) 3)
    (setq pass-count (+ pass-count 1)))

;; Calculate timing
(define elapsed1 (- end1 start1))
(define elapsed2 (- end2 start2))
(define elapsed3 (- end3 start3))
(define elapsed4 (- end4 start4))
(define elapsed5 (- end5 start5))

(print elapsed1)
(print elapsed2)
(print elapsed3)
(print elapsed4)
(print elapsed5)

;; Final result
(if (= pass-count test-count)
    (print "SUCCESS")
    (print "FAILURE"))
