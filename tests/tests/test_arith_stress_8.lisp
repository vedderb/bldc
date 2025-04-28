
(define n (if (is-always-gc) 100 100000))

(defun test-it (n c args res acc)
  (if (= n 0) acc
  (progn
    (define acc (and acc (= (apply c args) res)))
    (test-it (- n 1) c args res acc))))

(defun arith (a b c d e f g h i j)
  (+ a b c d e f g h i j))

(def res (test-it n arith '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0) 55.0 t))

(check res)
