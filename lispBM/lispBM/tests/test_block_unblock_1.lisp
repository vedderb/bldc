

(defun proc1 (pid)
  (progn
    (block)
    (send pid 'im-done)))


(def id (spawn proc1 (self)))
(unblock id)

(check (recv
        (im-done 't)))
