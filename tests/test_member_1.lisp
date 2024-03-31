

;; Check is pretty serious about t and nil
(check (eq (list 1 2 3) (and (member (list 1 2 3) 2)
                             (member (list 1 2 3) 3)
                             (member (list 1 2 3) 1))))
