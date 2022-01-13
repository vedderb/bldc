(let ((f (lambda ()
   (progn
     (define ppm (ppm-val)) ; So that lisp_stats shows the value
     (set-duty ppm)
     (reset-timeout)
     (yield 20000)
     (f)))))
(f))

