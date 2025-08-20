;; Simple Mutex Test

(define m (mutex-create))
(define lock-result (mutex-lock m))
(define unlock-result (mutex-unlock m))

(if (and (eq lock-result t) (eq unlock-result t))
    (print "SUCCESS")
    (print "FAILURE"))