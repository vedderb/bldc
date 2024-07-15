
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa "a couple of words in a row")
(define delim " ")

(gc)

(define n (mem-num-free))

(repeatq '(str-split apa delim) 1000)

(gc)

(check (= n (mem-num-free)))
