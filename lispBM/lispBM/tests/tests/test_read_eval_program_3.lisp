
(def prg "(defun test (x) (if (= x 2) 2 3)) (test 2)")

(check (= (read-eval-program prg) 2))
