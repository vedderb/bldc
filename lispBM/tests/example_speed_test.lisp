; Note: dec-cnt2 in C using a recursive function that takes
; a volatile int as argument takes 0.2 seconds to finish. 
; That is about 500x faster, which seems to be consistent
; with a few other tests.

; 12 seconds
(define dec-cnt (lambda (x)
  (if (= x 0) 0 (dec-cnt (- x 1)))
))

; 10 seconds
(define dec-cnt2 (lambda (x)
  (match x (0 0) (_ (dec-cnt2 (- x 1))))
))

; 4 seconds
(define dec-cnt3 (lambda (x)
  (if (> x 0) (dec-cnt3 (- (- (- (- (- (- (- x 1) 1) 1) 1) 1) 1) 1)) 0)
))

(define tak (lambda (x y z)
  (if (not (< y x))
      z
    (tak
     (tak (- x 1) y z)
     (tak (- y 1) z x)
     (tak (- z 1) x y)))))

(define fib (lambda (n)
  (if (< n 3) 1
    (+ (fib (- n 1)) (fib (- n 2))))))
    
(define q2 (lambda (x y)
  (if (or (< x 1) (< y 1)) 1
    (+ (q2 (- x (q2 (- x 1) y)) y)
       (q2 x (- y (q2 x (- y 1))))))))

(define f (lambda ()
   (progn
     
     (define start (systime))
     ;(dec-cnt3 100000)
     (define takres (tak 18 12 6))
     ;(define fibres (fib 23))
     ;(define q2res (q2 7 8))
     (print (list "Seconds elapsed: " (secs-since start)))
     
     (yield 2000000)
     (f)
)))
     
(f)

