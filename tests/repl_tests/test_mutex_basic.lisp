;; Mutex Extensions Basic Functionality Tests

;; Define cons? predicate
(define cons? (lambda (x)
  (and (list? x) (not (eq x nil)))))

(define test-count 0)
(define pass-count 0)

;; Test 1: Mutex creation
(define mutex1 (mutex-create))

(setq test-count (+ test-count 1))
(if (cons? mutex1)
    (setq pass-count (+ pass-count 1)))

;; Test 2: Mutex creation returns different objects
;; TODO: This needs a ptr comparison operator.
;;       unused mutexes have identical representation.
(define mutex2 (mutex-create))

;;(setq test-count (+ test-count 1))
(if (not (eq mutex1 mutex2))
    (setq pass-count (+ pass-count 1)))

;; Test 3: Lock and unlock basic functionality
(define lock-result (mutex-lock mutex1))
(define unlock-result (mutex-unlock mutex1))

(setq test-count (+ test-count 1))
(if (and (eq lock-result t) (eq unlock-result t))
    (setq pass-count (+ pass-count 1)))

;; Test 4: Multiple lock/unlock cycles
(define cycle-results '())
(define i 0)
(define cycle-success t)

;; Perform lock/unlock cycles
(define cycle-test (lambda () {
  (define lock-res (mutex-lock mutex1))
  (define unlock-res (mutex-unlock mutex1))
  (if (not (and (eq lock-res t) (eq unlock-res t)))
      (setq cycle-success nil))
  }))

(cycle-test)
(cycle-test)
(cycle-test)

(setq test-count (+ test-count 1))
(if cycle-success
    (setq pass-count (+ pass-count 1)))

;; Test 5: Lock state checking (indirect - through behavior)
;; Create a mutex, lock it, try basic operations
(define mutex3 (mutex-create))
(define first-lock (mutex-lock mutex3))

(setq test-count (+ test-count 1))
(if (eq first-lock t)
    (setq pass-count (+ pass-count 1)))

;; Test 6: Unlock without proper lock should fail
(define mutex4 (mutex-create))
(define invalid-unlock (trap (mutex-unlock mutex4)))

(setq test-count (+ test-count 1))
(if (not (eq invalid-unlock t))
    (setq pass-count (+ pass-count 1)))

;; Test 7: Multiple mutex independence
(define mutex-a (mutex-create))
(define mutex-b (mutex-create))
(define lock-a (mutex-lock mutex-a))
(define lock-b (mutex-lock mutex-b))
(define unlock-a (mutex-unlock mutex-a))
(define unlock-b (mutex-unlock mutex-b))

(setq test-count (+ test-count 1))
(if (and (eq lock-a t) (eq lock-b t) (eq unlock-a t) (eq unlock-b t))
    (setq pass-count (+ pass-count 1)))

;; Test 8: Mutex structure verification (basic cons pair check)
(define mutex5 (mutex-create))

(setq test-count (+ test-count 1))
(if (and (cons? mutex5) (eq (car mutex5) nil) (eq (cdr mutex5) nil))
    (setq pass-count (+ pass-count 1)))

;; Final result
(if (= pass-count test-count)
    (print "SUCCESS")
    (print "FAILURE"))
