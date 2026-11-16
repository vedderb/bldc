;; The two independent safety guards: LM_MAXCCALLS (recursion depth, e.g.
;; 150 nested capture groups exceeds LM_MAXCAPTURES well before it could
;; exceed the depth budget) and max_iterations (total backtracking work,
;; e.g. chained ".*" groups against a long non-matching string -- stays
;; at ~constant recursion depth so the depth guard alone would not catch
;; it). Both must return a clean error, not hang or crash.

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

(define nested-pattern (str-merge (str-replicate 150 40) "a" (str-replicate 150 41)))
(define t1 (check (car (trap (str-lua-match "a" nested-pattern))) 'exit-error 1))

(define catastrophic-pattern
  (str-merge ".*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*b"))
(define t2 (check (car (trap (str-lua-match (str-replicate 500 97) catastrophic-pattern))) 'exit-error 2))

(if (and t1 t2)
    (print "SUCCESS")
    (print "FAILURE"))
