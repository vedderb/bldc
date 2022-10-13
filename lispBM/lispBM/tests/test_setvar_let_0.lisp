
(defun q () 10)

(define b (let ((a 2))
            (progn 
              (setvar 'a (q))
              a)))


(= b 10)
