

(defun f () {
       (wait-for (shl 1 9))
       (check 't)
       })


(spawn f)


(trigger (shl 1 9))
