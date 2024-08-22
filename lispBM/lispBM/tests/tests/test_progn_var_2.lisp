
(defun nestled ()
  (progn
    (var a 10)
    (var b (progn
             (var b 20)
             (var a 100)
             (+ b a)))
    (+ a b)))

(check (= (nestled) 130))
