
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa "124.321")

(gc)

(define n (mem-num-free))

(repeatq '(str-to-f apa) 1000)

(gc)

(check (= n (mem-num-free)))
