


(defun zipwith (f xs ys)
  (let (( zip-acc (lambda (acc xs ys)                    
                    (if (and xs ys)
                        (zip-acc (cons (f (car xs) (car ys)) acc) (cdr xs) (cdr ys))
                      acc))))
    (reverse (zip-acc nil xs ys))))


(defun zip (xs ys)
  (zipwith cons xs ys))
