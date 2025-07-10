
(define syms [| apa bepa cepa kurt russel daniel jackson sam |])


(define rnd-sym (lambda ()
		  (ix syms (mod (random) 8)))) 

(define test-ok t)

(define recv-cnt 0)
(define send-cnt 100)
(define sent-cnt 0)

(define receiver
    (lambda () {
      (recv ((apa (? x)) (setq recv-cnt (+ recv-cnt 1)))
	    ((bepa (? x)) (setq recv-cnt (+ recv-cnt 1)))
	    ((cepa (? x)) (setq recv-cnt (+ recv-cnt 1)))
	    ((kurt (? x)) (setq recv-cnt (+ recv-cnt 1))) 
	    ((russel (? x)) (setq recv-cnt (+ recv-cnt 1)))
	    ((daniel (? x)) (setq recv-cnt (+ recv-cnt 1)))
	    ((jackson (? x)) (setq recv-cnt (+ recv-cnt 1)))
	    ((sam (? x)) (setq recv-cnt (+ recv-cnt 1)))
	    (_ (setq test-ok nil))
	    )
      (if (< recv-cnt send-cnt) 
	  (receiver)
	  ())
      }))

(define pid (spawn receiver))
(sleep 0.1)

(loopwhile (< sent-cnt send-cnt)
 {
 (define send-ok (send pid (list (rnd-sym) (random))))
 (setq sent-cnt (+ 1 sent-cnt))
 (sleep 0.01) ;; Give up some quota for receiver (or mailbox overflows)
 }
 )

(check (and (= recv-cnt send-cnt)
	    test-ok))
 
