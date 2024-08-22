(defun sleep (x)
  (yield (* x 1000000)))

(defun hello ()
  (progn
    (sleep 10)
    (print "hello")
    (hello)))


