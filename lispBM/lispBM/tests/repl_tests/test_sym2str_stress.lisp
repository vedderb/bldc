

(define r1 true)

(looprange i 0 10000
      (setq r1 (and r1 (eq (sym2str 'apa) "apa"))))


(if r1
    (print "SUCCESS")
    (print "FAILURE"))



