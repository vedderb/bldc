
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa "a very gentle string")

(gc)

(define n (mem-num-free))

(repeatq '(str-to-upper apa) 1000)

(gc)

(check (= n (mem-num-free)))
