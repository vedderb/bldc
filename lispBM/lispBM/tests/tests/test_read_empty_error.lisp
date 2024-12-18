

(def a (array-create 0))

(def e1 (trap (read [])))
(def e2 (trap (read-program [])))
(def e3 (trap (read a)))
(def e4 (trap (read-program a)))

(defun is_error (x)
  (eq x '(exit-error eval_error)))

(check (and (is_error e1)
            (is_error e2)
            (is_error e3)
            (is_error e4)))
