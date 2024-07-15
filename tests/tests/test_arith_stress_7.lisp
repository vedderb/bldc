
(defun apply (f args)
  (eval (cons f args)))

(defun test-it (n c args res acc)
  (if (= n 0) acc
  (progn
    (define acc (and acc (= (apply c args) res)))
    (test-it (- n 1) c args res acc))))

(defun arith (a b c d e f g h i j)
  (+ a b c d e f g h i j))

(def res (test-it 10000 arith (list
                               (str-to-i "1")
                               (str-to-i "2")
                               (str-to-i "3")
                               (str-to-i "4")
                               (str-to-i "5")
                               (str-to-i "6")
                               (str-to-i "7")
                               (str-to-i "8")
                               (str-to-i "9")
                               (str-to-i "10"))
                  (str-to-i "55") t))

(check res)
