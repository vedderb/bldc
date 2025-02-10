

(defun punisher (n arg)
  (if (= n 0) arg
    (progn
      (let (( (a  (b0 _ b2) . c) '(1 (100 200 300) . 77) ))
        (punisher (- n 1) (+ a b0 b2 c))
        )
      )
    )
  )


(define iterations (if (is-always-gc) 10 100000))

(check (= (punisher iterations 0) 478))
