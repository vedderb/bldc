
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa "apa")
(define bepa "bepa")

(gc)

(define n (mem-num-free))

(repeatq '(str-merge apa bepa) 1000)

(gc)

(check (= n (mem-num-free)))
