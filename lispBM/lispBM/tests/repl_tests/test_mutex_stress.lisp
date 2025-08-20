;; Mutex Extensions Stress Tests

;; Define cons? predicate
(define cons? (lambda (x)
  (and (list? x) (not (eq x nil)))))

(define null? (lambda (x)
                (eq nil x)))

(define test-count 0)
(define pass-count 0)

;; Test 1: Many mutex creations
(define mutex-list '())
(define create-mutexes (lambda (n)
  (if (> n 0)
      (progn
        (setq mutex-list (cons (mutex-create) mutex-list))
        (create-mutexes (- n 1))))))

(create-mutexes 20)

(setq test-count (+ test-count 1))
(if (= (length mutex-list) 20)
    (setq pass-count (+ pass-count 1)))

;; Test 2: Rapid lock/unlock cycles on multiple mutexes
(define rapid-cycle-success t)
(define rapid-test (lambda (mutex-lst)
  (if (not (null? mutex-lst))
      (progn
        (define lock-res (mutex-lock (car mutex-lst)))
        (define unlock-res (mutex-unlock (car mutex-lst)))
        (if (not (and (eq lock-res t) (eq unlock-res t)))
            (setq rapid-cycle-success nil))
        (rapid-test (cdr mutex-lst))))))

(rapid-test mutex-list)

(setq test-count (+ test-count 1))
(if rapid-cycle-success
    (setq pass-count (+ pass-count 1)))

;; Test 3: Nested mutex operations (careful with deadlock potential)
(define mutex-outer (mutex-create))
(define mutex-inner (mutex-create))

(define nested-success t)
(define outer-lock (mutex-lock mutex-outer))
(define inner-lock (mutex-lock mutex-inner))
(define inner-unlock (mutex-unlock mutex-inner))
(define outer-unlock (mutex-unlock mutex-outer))

(if (not (and (eq outer-lock t) (eq inner-lock t) 
              (eq inner-unlock t) (eq outer-unlock t)))
    (setq nested-success nil))

(setq test-count (+ test-count 1))
(if nested-success
    (setq pass-count (+ pass-count 1)))

;; Test 4: Lock/unlock pattern verification
(define pattern-mutex (mutex-create))
(define pattern-results '())

;; Perform a specific pattern of operations
(define lock1 (mutex-lock pattern-mutex))
(setq pattern-results (cons lock1 pattern-results))

(define unlock1 (mutex-unlock pattern-mutex))
(setq pattern-results (cons unlock1 pattern-results))

(define lock2 (mutex-lock pattern-mutex))
(setq pattern-results (cons lock2 pattern-results))

(define unlock2 (mutex-unlock pattern-mutex))
(setq pattern-results (cons unlock2 pattern-results))

;; All operations should succeed (return t)
(define all-true (lambda (lst)
  (if (null? lst)
      t
      (if (eq (car lst) t)
          (all-true (cdr lst))
          nil))))

(setq test-count (+ test-count 1))
(if (all-true pattern-results)
    (setq pass-count (+ pass-count 1)))

;; Test 5: Error condition testing - multiple unlocks
(define error-mutex (mutex-create))
(define first-lock (mutex-lock error-mutex))
(define first-unlock (mutex-unlock error-mutex))
(define second-unlock (trap (mutex-unlock error-mutex)))  ;; Should fail

(setq test-count (+ test-count 1))
(if (and (eq first-lock t) (eq first-unlock t) (not (eq second-unlock t)))
    (setq pass-count (+ pass-count 1)))

;; Test 6: Mutex with functional operations
(define func-mutexes (map (lambda (x) (mutex-create)) (range 0 5)))
(define lock-results (map mutex-lock func-mutexes))
(define unlock-results (map mutex-unlock func-mutexes))

(setq test-count (+ test-count 1))
(if (and (= (length lock-results) 5) (= (length unlock-results) 5)
         (all-true lock-results) (all-true unlock-results))
    (setq pass-count (+ pass-count 1)))

;; Test 7: Performance timing of mutex operations
(define perf-mutex (mutex-create))
(define start-time (systime))

;; Perform many lock/unlock operations
(define perf-operations (lambda (n)
  (if (> n 0)
      (progn
        (mutex-lock perf-mutex)
        (mutex-unlock perf-mutex)
        (perf-operations (- n 1))))))

(perf-operations 50)
(define end-time (systime))
(define elapsed (- end-time start-time))

(setq test-count (+ test-count 1))
;; Just verify operations completed (elapsed should be > 0)
(if (> elapsed 0)
    (setq pass-count (+ pass-count 1)))

;; Final result
(if (= pass-count test-count)
    (print "SUCCESS")
    (print "FAILURE"))
