



(defun apply (f args)
  (eval (cons f args)))

(defun test-it (n c args res acc)
  (if (= n 0) acc
  (progn
    (define acc (and acc (= (apply c args) res)))
    (test-it (- n 1) c args res acc))))

(defun arith (a b c d e f g h i j)
  (+ a b c d e f g h i j))

;(def n-gc1 (lbm-heap-state 'get-gc-num))

(def res (test-it 10000 arith '(1 2 3 4 5 6 7 8 9 10) 55 t))

;(def n-gc2 (lbm-heap-state 'get-gc-num))

(check res)

;(list n-gc1 n-gc2)
