

(check (let ((a (lambda (x) (if (= x 0) 1 (a (- x 1))))))
         (let ((a (lambda (x) (if (= x 0) 2 (a ( - x 1))))))
           (= (a 1) 2))))

