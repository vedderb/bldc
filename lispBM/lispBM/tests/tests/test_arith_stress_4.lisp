
(defun apply (f args)
  (eval (cons f args)))

(defun test-it (n c args res acc)
  (if (= n 0) acc
  (progn
    (define acc (and acc (= (apply c args) res)))
    (test-it (- n 1) c args res acc))))

(defun arith (a b c d e f g h i j)
  (+ a b c d e f g h i j))

(def res (test-it 10000 arith '(1u64 2u64 3u64 4u64 5u64 6u64 7u64 8u64 9u64 10u64) 55u64 t))

(check res)
