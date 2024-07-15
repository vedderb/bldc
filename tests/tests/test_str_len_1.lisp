
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))

(define my-string "anjovis")

(gc)

(define n (mem-num-free))

(repeatq '(str-len my-string) 1000)

(gc)

(check (= n (mem-num-free)))
