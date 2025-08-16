
(define set1 nil)
(define set2 nil)
(define u nil)

;; create enough single element sets to trigger GC
;; Not guaranteed to trigger, but likely.
(looprange i 0 10000 {
      (set-insert nil i)
      (set-insert nil i)
      (set-insert nil i)
      (set-insert nil i)
      (set-insert nil i)
      (set-insert nil i)
      (set-insert nil i)
      (set-insert nil i)
      })
      

;; create and remember 2 sets of 10 elements
(looprange i 0 10 {
      (setq set1 (set-insert set1 i))
      (setq set2 (set-insert set2 (+ 10 i) ))
      })

;; perform union of 2 10 element sets enough times for GC.
(looprange i 0 1000
      (setq u (set-union set1 set2)))

(if (and (member 1 u )
         (member 11 u))
    (print "SUCCESS")
    (print "FAILURE"))


      
