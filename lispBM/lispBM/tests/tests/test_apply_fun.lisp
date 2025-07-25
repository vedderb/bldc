(defun fun (arg) (cons arg (rest-args)))
(defun fun-no-args () (rest-args))

(defun test (a b c) (list a b c))

(check (and
    (eq (apply fun '(a)) '(a))
    (eq (apply fun '(a b)) '(a b))
    (eq (apply fun-no-args '()) nil)
    (eq (apply fun-no-args '(a b)) '(a b))
    (eq (apply list '(a b)) '(a b))
    (eq (apply and '(t nil)) nil)
    (= (apply + '(1 2 3)) 6)
    (eq (apply fun (iota 3)) '(0 1 2))
    (eq (apply (closure (a b) (list a b)) (iota 2)) '(0 1))
))

