
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa "124")

(gc)

(define n (mem-num-free))

(repeatq '(str-to-i apa) 1000)

(gc)

(check (= n (mem-num-free)))
