
(def a 0)

(defun g () {
       (wait-for (shl 1 9))
       (def a (+ a 1))
       })

(defun f () {
       (wait-for (shl 1 9))
       (def a (+ a 1))
       })

(spawn f)
(spawn g)
(yield 10000)
(trigger (shl 1 9))
(yield 10000)
(check (= a 2))
