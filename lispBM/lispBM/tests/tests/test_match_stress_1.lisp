


(define syms [| apa bepa cepa kurt russel daniel jackson sam |])

(define rnd-sym (lambda ()
		  (ix syms (mod (random) 8)))) 

(define test-ok t)

;; Always gc takes ages on this test if loop over 10 iterations.
(define iterations (if (is-always-gc) 10 100))

(loopfor i 0 (< i iterations) (+ i 1)
      (match (list (rnd-sym) (random))
	     ( (apa (? x)) (> x 1000) (setq test-ok (and t test-ok)))
	     ( (apa (? x)) (<= x 1000) (setq test-ok (and t test-ok)))
	     ( (bepa (? x)) (> x 2000) (setq test-ok (and t test-ok)))
	     ( (bepa (? x)) (<= x 2000) (setq test-ok (and t test-ok)))
	     ( (cepa (? x)) (> x 3000) (setq test-ok (and t test-ok)))
	     ( (cepa (? x)) (<= x 3000) (setq test-ok (and t test-ok)))
	     ( (kurt (? x)) (> x 4000) (setq test-ok (and t test-ok)))
	     ( (kurt (? x)) (<= x 4000) (setq test-ok (and t test-ok)))
	     ( (russel (? x)) (> x 5000) (setq test-ok (and t test-ok)))
	     ( (russel (? x)) (<= x 5000) (setq test-ok (and t test-ok)))
	     ( (daniel (? x)) (> x 6000) (setq test-ok (and t test-ok)))
	     ( (daniel (? x)) (<= x 6000) (setq test-ok (and t test-ok)))
	     ( (jackson (? x)) (> x 7000) (setq test-ok (and t test-ok)))
	     ( (jackson (? x)) (<= x 7000) (setq test-ok (and t test-ok)))
	     ( (sam (? x)) (> x 8000) (setq test-ok (and t test-ok)))
	     ( (sam (? x)) (<= x 8000) (setq test-ok (and t test-ok)))
	     ( _ (setq test-ok nil)))
      )


(check test-ok)
