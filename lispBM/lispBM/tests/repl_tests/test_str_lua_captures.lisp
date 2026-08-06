;; Captures: single/multiple/nested/position, and %1-%9 backreferences
;; within a pattern.

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

(define t1 (check (str-lua-match "key=value" "(%a+)=(%a+)") (list "key" "value") 1))
(define t2 (check (str-lua-match "aaaa" "(a)(a)(a)(a)") (list "a" "a" "a" "a") 2))
;; position captures are 0-based, matching str-lua-find's own start/end for
;; the same match (str-lua-find "hello" "ll") -> (2 4 "ll") -- not Lua's
;; own 1-based convention.
(define t3 (check (str-lua-match "hello" "()ll()") (list 2 4) 3))
(define t4 (check (str-lua-match "abcabc" "(abc)%1") (list "abc") 4))
(define t5 (check (str-lua-match "abcxyz" "(abc)%1") nil 5))

(if (and t1 t2 t3 t4 t5)
    (print "SUCCESS")
    (print "FAILURE"))
