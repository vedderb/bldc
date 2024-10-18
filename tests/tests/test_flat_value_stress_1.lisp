
(defun test (n)
  {
  (var a (range n))
  (var b (flatten a))
  (var c (unflatten b))
  (eq (range n) c)
  }
  )


(defun tests-n-to-1 (n)
  (if (= n 0) t
      {
      (var r (test n))
      (if r (tests-n-to-1 (- n 1))
	  nil)
      }
      )
  )

(defun run-tests (n)
  (if (= n 0) t
      {
      (var r (tests-n-to-1 10))
      (if r (run-tests (- n 1))
	  nil
	  )
      }
      )
  )

(check (run-tests 10))



