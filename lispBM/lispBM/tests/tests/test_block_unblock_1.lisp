

(defun proc1 (pid)
  (progn
    (block)
    (send pid 'im-done)))


(def id (spawn proc1 (self)))
(sleep 0.1) ;; give proc1 time to start up and block
(unblock id)

(check (recv
        (im-done 't)))
