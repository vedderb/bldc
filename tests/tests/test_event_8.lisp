

(event-register-handler (self))

(defun event-sender (n)
  (if (= 0 n) ()
    (progn
      (event-list-of-float 3.14 3.14 3.14)
      (iota 250) ; Trigger GC eventually
      (event-sender (- n 1)))))
    
(spawn event-sender 100)

(defun correct (n)
  (if (= n 0) t
    (progn
      (recv ((? x) (if (eq x '(3.14 3.14 3.14))
                      (correct (- n 1))
                    x))))))

(check (correct 70))
