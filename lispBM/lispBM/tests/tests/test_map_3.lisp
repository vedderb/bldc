;; (let ((accmap (lambda (f acc xs)
;; 		(if (eq xs nil)
;; 		    acc
;; 		  (accmap f (cons (f (car xs)) acc) (cdr xs))))))
;;   (define map (lambda (f xs) (reverse (accmap f nil xs)))))
  
(check (eq (map (lambda (x) (+ x 1)) (list 1 2 3 4 5 6)) (list 2 3 4 5 6 7)))
