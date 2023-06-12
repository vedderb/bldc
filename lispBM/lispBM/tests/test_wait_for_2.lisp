

(defun g () {
       (wait-for (shl 1 3))
       (check 't)
       })

(defun f () {
       (wait-for (shl 1 9))
       (trigger (shl 1 3))
       })

(spawn f)
(spawn g)

(trigger (shl 1 9))
