
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa "Hello world")

(gc)

(define n (mem-num-free))

(repeatq '(str-part apa 4) 1000)

(gc)

(check (= n (mem-num-free)))
