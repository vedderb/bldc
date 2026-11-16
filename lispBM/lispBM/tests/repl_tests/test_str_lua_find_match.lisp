;; Basic literal/quantifier/anchor/character-class matching for str-lua-find
;; and str-lua-match (the ported Lua pattern-matching engine).

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

(define t1  (check (str-lua-find "hello world" "wor") (list 6 9 "wor") 1))
(define t2  (check (str-lua-find "hello world" "xyz") nil 2))
(define t3  (check (str-lua-match "aaa" "a*a") "aaa" 3))
(define t4  (check (str-lua-match "" "a*") "" 4))
(define t5  (check (str-lua-match "abc" "^abc$") "abc" 5))
(define t6  (check (str-lua-match "xabc" "^abc$") nil 6))
(define t7  (check (str-lua-match "abc123" "%a+") "abc" 7))
(define t8  (check (str-lua-match "abc123" "%d+") "123" 8))
(define t9  (check (str-lua-match "color" "colou?r") "color" 9))
(define t10 (check (str-lua-match "colour" "colou?r") "colour" 10))
(define t11 (check (str-lua-match "Hello_123 world" "[A-Za-z_][A-Za-z0-9_]*") "Hello_123" 11))
(define t12 (check (str-lua-match "  123.45  " "%d+%.?%d*") "123.45" 12))
(define t13 (check (str-lua-match "THE QUICK" "%u+") "THE" 13))
(define t14 (check (str-lua-match "the quick" "%l+") "the" 14))
(define t15 (check (str-lua-match "abc [xyz] def" "%[[^%]]*%]") "[xyz]" 15))
(define t16 (check (str-lua-match "(foo (bar) baz)" "%b()") "(foo (bar) baz)" 16))
(define t17 (check (str-lua-match "THE (quick) fox" "%f[%a]%u+%f[%A]") "THE" 17))

(if (and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17)
    (print "SUCCESS")
    (print "FAILURE"))
