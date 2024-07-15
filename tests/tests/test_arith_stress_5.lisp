
(defun apply (f args)
  (eval (cons f args)))

(defun test-it (n c args res acc)
  (if (= n 0) acc
  (progn
    (define acc (and acc (= (apply c args) res)))
    (test-it (- n 1) c args res acc))))

(defun arith (a b c d e f g h i j)
  (+ a b c d e f g h i j))

(def res (test-it 10000 arith '(1i64 2i64 3i64 4i64 5i64 6i64 7i64 8i64 9i64 10i64) 55i64 t))

(check res)
