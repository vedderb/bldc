
(define a 1i64)

(defun f () 
  (loop ((i 0))
        (< i 10000)
        {
        (setq a (+ a 1))
        (setq i (+ i 1))
        }))

(gc)
(define n (mem-num-free))

(f)

(gc)
(check (= n (mem-num-free)))
