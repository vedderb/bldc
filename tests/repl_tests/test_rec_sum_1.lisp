
(defun sum-up (ls)
  (if (eq ls nil) 0
    (+ (car ls) (sum-up (cdr ls)))))


(if (= (sum-up (range 11)) 55) (print "SUCCESS") (print "FAILURE"))
