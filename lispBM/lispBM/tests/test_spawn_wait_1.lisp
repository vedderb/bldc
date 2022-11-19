
(define t1 (lambda (x)
             (+ x 1)))



(let ((id (spawn t1 0)))
  (progn
    (wait id)
    'true))
