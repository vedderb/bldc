;; A byte-array containing an embedded NUL ("AB\0CD") is searched safely and
;; consistently with the rest of the string library: content before the NUL
;; is found, content after it is not (strlen_max semantics, matching
;; str-join/str-split/etc, not a limitation to fix).

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

(define b (bufcreate 5))
(bufset-u8 b 0 65)
(bufset-u8 b 1 66)
(bufset-u8 b 2 0)
(bufset-u8 b 3 67)
(bufset-u8 b 4 68)

(define t1 (check (str-lua-find b "B") (list 1 2 "B") 1))
(define t2 (check (str-lua-find b "C") nil 2))

(if (and t1 t2)
    (print "SUCCESS")
    (print "FAILURE"))
