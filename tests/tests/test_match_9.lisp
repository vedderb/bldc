(defun test (x) x)

(check ( = (let ((a 1))
             (match (test 75)
                    (75  a)))
           1))

;; (= (let ( (a 1) )
;;      (test a))
;;    1)
