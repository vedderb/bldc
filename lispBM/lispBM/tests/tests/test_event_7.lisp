
;; If run at mailbox size 10 this can be scheduled such that some mail is never delivered.
(set-mailbox-size 100)

(event-register-handler (self))

(defun event-sender (n)
  (if (= 0 n) ()
    (progn
      (event-float 3.14)
      (iota 250) ; Trigger GC eventually
      (event-sender (- n 1)))))
    
(spawn event-sender 100)



(check (and (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            (recv ((? x) (eq x 3.14)))
            ))
     
