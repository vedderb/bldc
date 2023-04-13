

(defun tail-rec (n acc)
  (if (= n 0) acc
  (progn
    (var sum (+ acc n))
    (tail-rec (- n 1) sum))))
    
(check (= (tail-rec 10000 0) 50005000))
