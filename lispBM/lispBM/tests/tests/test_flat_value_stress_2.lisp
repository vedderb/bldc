
(define orig (list 12u32
		   "kurt russel"
		   1b
		   2b
		   "is great"
		   [1 2 3 4 5]
		   55i32
		   24
		   'apa
		   "spongebob"
		   ))


(defun test (n)
  {
  (var a (take orig n))
  (var b (flatten a))
  (var c (unflatten b))
  (eq (take orig n) c)
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
