
(defun proc1 (pid)
  (progn
    (block)
    (send pid 'im-done)))


(def id (spawn-trap proc1 (self)))
(unblock-error id)

(check (recv ((exit-error (? tid) (? e)) 't)
             ((exit-ok    (? tid) (? r)) 'nil)
             (im-done 'this-can-not-happen)))
