(def prg "(defun test (x) (cond ((= x 1) 1) (t 2))) (test 1)")


(check (= (read-eval-program prg) 1))
