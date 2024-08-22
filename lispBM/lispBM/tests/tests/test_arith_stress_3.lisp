
(defun apply (f args)
  (eval (cons f args)))

(defun test-it (n c args res acc)
  (if (= n 0) acc
  (progn
    (define acc (and acc (= (apply c args) res)))
    (test-it (- n 1) c args res acc))))

(defun arith (a b c d e f g h i j)
  (+ a b c d e f g h i j))

(def res (test-it 10000 arith '(1u32 2u32 3u32 4u32 5u32 6u32 7u32 8u32 9u32 10u32) 55u32 t))

(check res)
