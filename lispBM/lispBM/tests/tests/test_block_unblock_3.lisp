

(defun proc1 (pid)
  (progn
    (define apa (block-rmbr))
    (send pid apa)))

(def id (spawn proc1 (self)))

(sleep 1)

(gc)

(unblock-rmbr id)

(recv ( (? x) (def res x)))

(check (eq res [1 2 3 4] ))

