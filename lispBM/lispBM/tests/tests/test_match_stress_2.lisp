

(define syms [| apa bepa cepa kurt russel daniel jackson sam |])

(define rnd-sym (lambda ()
		  (ix syms (mod (random) 8)))) 

(define test-ok t)

;; Always gc takes ages on this test if loop over 2 iterations.

(define iterations (if (is-always-gc) 2 100))

(loopfor i 0 (< i iterations) (+ i 1)
      (match (list (rnd-sym) (random) (random))
	     ( (apa (? x) (? y) ) (and (> x 1000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (apa (? x) (? y) ) (and (<= x 1000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (bepa (? x) (? y) ) (and (> x 2000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (bepa (? x) (? y) ) (and (<= x 2000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (cepa (? x) (? y) ) (and (> x 3000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (cepa (? x) (? y) ) (and (<= x 3000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (kurt (? x) (? y) ) (and (> x 4000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (kurt (? x) (? y) ) (and (<= x 4000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (russel (? x) (? y) ) (and (> x 5000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (russel (? x) (? y) ) (and (<= x 5000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (daniel (? x) (? y) ) (and (> x 6000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (danial (? x) (? y) ) (and (<= x 6000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (jackson (? x) (? y) ) (and (> x 7000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (jackson (? x) (? y) ) (and (<= x 7000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (sam (? x) (? y) ) (and (> x 8000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( (sam (? x) (? y) ) (and (<= x 8000) (eq (type-of y) type-u)) (setq test-ok (and t test-ok)))
	     ( _ (setq test-ok nil)))
      )


(check test-ok)
