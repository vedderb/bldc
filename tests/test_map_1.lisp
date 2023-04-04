;; (define map (lambda (f xs)
;; 	      (let ((accmap (lambda (acc xs)
;; 			      (if (eq xs nil)
;; 				  acc
;; 				(accmap (cons (f (car xs)) acc) (cdr xs))))))
;; 		(reverse (accmap nil xs)))))

(check (eq (map (lambda (x) (+ x 1)) (list 1 2 3 4 5 6)) (list 2 3 4 5 6 7)))

		
