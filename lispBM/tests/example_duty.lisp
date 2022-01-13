(define upcnt 1)

(let ((f (lambda (x)
   (progn
     (set-duty (* 0.001 x))
     (reset-timeout)
     (yield 10000)
     
     (if (> x 1000) (define upcnt (- 1)) nil)
     (if (< x (- 1000)) (define upcnt 1) nil)
     
     (f (+ x upcnt))))))
(f 0))

