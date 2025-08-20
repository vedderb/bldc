

(defun f () {
       (sleep 10)
       (f)
       }
  )


(define pid (spawn f))

(send pid 1)
(send pid 2)
(send pid 3)

(define ls (mailbox-get pid))

(print ls)

(if (eq ls (list 1 2 3))
    (print "SUCCESS")
  (print "FAILURE")
  )

