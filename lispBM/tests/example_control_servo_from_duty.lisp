(define servo-out 0.5)

(define abs (lambda (x) (if (> x 0) x (- x))))

(define update-servo (lambda ()
  (progn
    (define servo-out (+ servo-out (* (get-duty) 0.2)))
    (if (> servo-out 1.0) (define servo-out 1.0) nil)
    (if (< servo-out 0) (define servo-out 0) nil)
    (set-servo servo-out)
)))

(define itcnt 0)

(define f (lambda ()
   (progn
     (define itcnt (+ itcnt 1))
     (if (> (abs (get-duty)) 0.005) (update-servo) nil)
     (yield 10000)
     (f)
)))
     
(f)

