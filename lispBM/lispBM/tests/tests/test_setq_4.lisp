
(defun f (x) {
       (var a 10)
       (setq a 100)
       (+ x a)
       })

(check (= (f 1) 101))
