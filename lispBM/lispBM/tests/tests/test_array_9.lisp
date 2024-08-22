
(define a [ 1 2 3 4 5 6 7 8 9])

(defun extract (arr s e)
  (let ((f (lambda (arr x) (bufget-u8 arr x))))
    ( map (lambda (x) (f arr x)) (range s e))))

(check (eq (extract a 0 4) '(1 2 3 4)))
