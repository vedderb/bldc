
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))

(define delim "anjovis")

(gc)

(define n (mem-num-free))

(repeatq '(to-str-delim delim 1 2 3 4 5 6 7 8 9) 1000)

(gc)

(check (= n (mem-num-free)))
