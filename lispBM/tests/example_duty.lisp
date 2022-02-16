;;; This program will ramp the duty cycle up and down according
;;; to the parameters below.

(define rate 50) ; Update rate in hz
(define ramptime 3.0) ; Motor ramp time

(define rampstep (/ 1.0 (* rate ramptime)))
(define upcnt rampstep)

(let ((f (lambda (x)
   (progn
     (set-duty x)
     (timeout-reset)
     
     (yield (/ 1000000 rate))
     
     (if (> x 1.0) (define upcnt (- rampstep) nil))
     (if (< x (- 1.0)) (define upcnt rampstep) nil)
     
     (f (+ x upcnt))
)))) (f 0))

