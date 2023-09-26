
(defun f (x) {
       (var a 10)
       (var b 20)
       (var a 100)
       (+ a b x)
       })

(check (= (f 1) 121))
