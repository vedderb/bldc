
(define a (list 1 2 3 4))
(define b (cons [1 2 3 4] [5 6 7 8]))
(define c "hello world")
(define d (flatten (cons c b)))

(defun f () {
  (looprange i 0 25
             (gc))
  'ok})


(check (eq (f) 'ok))
  
