;; Malformed patterns must return a clean error (via trap), not crash.

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

(define t1 (check (car (trap (str-lua-match "abc" "abc%"))) 'exit-error 1))
(define t2 (check (car (trap (str-lua-match "abc" "[abc"))) 'exit-error 2))
(define t3 (check (car (trap (str-lua-match "abc" "%b("))) 'exit-error 3))
(define t4 (check (car (trap (str-lua-match "abc" "%f"))) 'exit-error 4))

(if (and t1 t2 t3 t4)
    (print "SUCCESS")
    (print "FAILURE"))
