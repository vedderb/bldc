
(defun repeatq (f n)
  (if ( = n 0)
      ()
    (progn
      f
      (repeatq f (- n 1)))))


(define apa  "a string that is quite unique")
(define bepa "a string tat is quite oonique")

(gc)

(define n (mem-num-free))

(repeatq '(str-cmp apa bepa) 1000)

(gc)

(check (= n (mem-num-free)))
