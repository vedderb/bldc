(define big-list 
  (range 0 100))

(define sum-result 
  (foldl + 0 big-list))

(define nested-list
  (map (lambda (x) (list x (* x 2) (* x 3))) (range 0 10)))

(define flat-result
  (foldl append nil nested-list))

(if (and (= sum-result 4950)
         (= (length flat-result) 30))
    (print "SUCCESS")
  (print "FAILURE"))

