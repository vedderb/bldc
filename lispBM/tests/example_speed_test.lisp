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

(define f (lambda ()
   (progn
     
     (define start (systime))
     (dec-cnt2 100000)
     (print (list "Seconds elapsed: " (secs-since start)))
     
     (yield 2000000)
     (f)
)))
     
(f)

