
(defun proc1 (pid)
  (progn
    (var a (block))
    
    (send pid a)))


(def id (spawn-trap proc1 (self)))

(send id 1) ;; mess with the process

(sleep 0.1)
(define res (unblock id))




(check (and (recv ((exit-error (? tid) (? e)) 'nil)
                  ((exit-ok    (? tid) (? r)) 'nil)
                  ((? x)  x))
            res))
