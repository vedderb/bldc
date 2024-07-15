
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa "a couple of words in a row")
(define r "words")
(define w "penguins")

(gc)

(define n (mem-num-free))

(repeatq '(str-replace apa r w) 1000)

(gc)

(check (= n (mem-num-free)))
