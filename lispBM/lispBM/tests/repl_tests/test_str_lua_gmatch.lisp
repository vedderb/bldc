;; str-lua-gmatch: explicit resumable search state, threaded across calls
;; as a plain integer (not a closure -- see the design notes in
;; pattern_extensions.c). Also covers the empty-match-skip logic.

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

(define t1 (check (str-lua-gmatch "one two three" "%a+" nil) (cons "one" 3) 1))
(define t2 (check (str-lua-gmatch "one two three" "%a+" 3) (cons "two" 7) 2))
(define t3 (check (str-lua-gmatch "one two three" "%a+" 7) (cons "three" 13) 3))
(define t4 (check (str-lua-gmatch "one two three" "%a+" 13) nil 4))
(define t5 (check (str-lua-gmatch "baaabab" "a*" nil) (cons "" 0) 5))

;; collecting all matches by looping until nil, using the returned
;; next-state directly -- the intended calling idiom.
(defunret collect-all (text pattern)
  (progn
    (var acc nil)
    (var state nil)
    (loopwhile t
      (progn
        (var r (str-lua-gmatch text pattern state))
        (if (eq r nil)
            (return (reverse acc))
            (progn
              (setq acc (cons (car r) acc))
              (setq state (cdr r))))))))

(define t6 (check (collect-all "one two three" "%a+") (list "one" "two" "three") 6))

(if (and t1 t2 t3 t4 t5 t6)
    (print "SUCCESS")
    (print "FAILURE"))
