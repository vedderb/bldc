
(def x 1)
(def y 2)
(def z 3)

(defun f (x y z) {
       (setq x 10)
       (setq y 20)
       (setq z 30)
       (+ x y z)
       })

(check (and (= (f 0 0 0) 60)
            (= x 1)
            (= y 2)
            (= z 3)))
