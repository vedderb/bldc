(let ((f (lambda ()
   (progn
     (define ppm (get-ppm)) ; So that lisp_stats shows the value
     (set-duty ppm)
     (timeout-reset)
     (yield 20000)
     (f)))))
(f))

