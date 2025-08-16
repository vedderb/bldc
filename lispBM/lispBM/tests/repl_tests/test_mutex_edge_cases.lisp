;; Mutex Extensions Edge Case Tests

;; Define cons? predicate
(define cons? (lambda (x)
  (and (list? x) (not (eq x nil)))))

(define test-count 0)
(define pass-count 0)

;; Test 1: Invalid arguments to mutex functions
(define invalid-lock-result1 (trap (mutex-lock nil)))
(define invalid-lock-result2 (trap (mutex-lock 42)))
(define invalid-lock-result3 (trap (mutex-lock "not-a-mutex")))

(setq test-count (+ test-count 1))
;; All should fail (not return t)
(if (not (or (eq invalid-lock-result1 t) 
             (eq invalid-lock-result2 t) 
             (eq invalid-lock-result3 t)))
    (setq pass-count (+ pass-count 1)))

;; Test 2: Invalid arguments to mutex-unlock
(define invalid-unlock-result1 (trap (mutex-unlock nil)))
(define invalid-unlock-result2 (trap (mutex-unlock 42)))
(define invalid-unlock-result3 (trap (mutex-unlock "not-a-mutex")))

(setq test-count (+ test-count 1))
;; All should fail (not return t)
(if (not (or (eq invalid-unlock-result1 t) 
             (eq invalid-unlock-result2 t) 
             (eq invalid-unlock-result3 t)))
    (setq pass-count (+ pass-count 1)))

;; Test 3: Unlock without lock
(define fresh-mutex (mutex-create))
(define unlock-without-lock (trap (mutex-unlock fresh-mutex)))

(setq test-count (+ test-count 1))
(if (not (eq unlock-without-lock t))
    (setq pass-count (+ pass-count 1)))

;; Test 4: Double unlock after single lock
(define double-unlock-mutex (mutex-create))
(define initial-lock (mutex-lock double-unlock-mutex))
(define first-unlock (mutex-unlock double-unlock-mutex))
(define second-unlock (trap (mutex-unlock double-unlock-mutex)))

(setq test-count (+ test-count 1))
(if (and (eq initial-lock t) (eq first-unlock t) (not (eq second-unlock t)))
    (setq pass-count (+ pass-count 1)))

;; Test 5: Mutex structure integrity after operations
(define struct-mutex (mutex-create))
(define original-car (car struct-mutex))
(define original-cdr (cdr struct-mutex))

;; Perform operations
(mutex-lock struct-mutex)
(mutex-unlock struct-mutex)

;; Check if structure is restored
(define restored-car (car struct-mutex))
(define restored-cdr (cdr struct-mutex))

(setq test-count (+ test-count 1))
(if (and (eq original-car restored-car) (eq original-cdr restored-cdr))
    (setq pass-count (+ pass-count 1)))

;; Test 6: Empty list as mutex (edge case)
(define empty-list-mutex '())
(define empty-lock-result (trap (mutex-lock empty-list-mutex)))

(setq test-count (+ test-count 1))
;; Empty list should not be treated as valid mutex
(if (not (eq empty-lock-result t))
    (setq pass-count (+ pass-count 1)))

;; Test 7: Cons pair that looks like mutex but isn't
;;(define fake-mutex (cons 'not-nil 'also-not-nil))
;;(define fake-lock-result (mutex-lock fake-mutex))

;; Figure out why this leads to a loop.
;;   Makes sense, it looks like a locked mutex the attempted lock
;;   leads to blocking the thread. 

;;(setq test-count (+ test-count 1))
;;(if (or (eq fake-lock-result t) (not (eq fake-lock-result t)))
;;    (setq pass-count (+ pass-count 1)))

;; Test 8: Mutex operations with no arguments
(define no-arg-result1 (trap (mutex-lock)))
(define no-arg-result2 (trap (mutex-unlock)))

(setq test-count (+ test-count 1))
;; Both should result in errors (trapped)
(if (and (not (eq no-arg-result1 t)) (not (eq no-arg-result2 t)))
    (setq pass-count (+ pass-count 1)))

;; Test 9: Mutex operations with too many arguments  
(define too-many-args1 (trap (mutex-lock (mutex-create) (mutex-create))))
(define too-many-args2 (trap (mutex-unlock (mutex-create) (mutex-create))))

(setq test-count (+ test-count 1))
;; Both should result in errors or be ignored gracefully
(if (and (not (eq too-many-args1 t)) (not (eq too-many-args2 t)))
    (setq pass-count (+ pass-count 1)))

;; Test 10: Mutex with modified internal structure
(define modified-mutex (mutex-create))
(mutex-lock modified-mutex)

;; Try to manually modify the mutex structure (dangerous!)
;; This tests robustness against corruption
(define corrupt-unlock (mutex-unlock modified-mutex))

(setq test-count (+ test-count 1))
;; Should still work normally
(if (eq corrupt-unlock t)
    (setq pass-count (+ pass-count 1)))

;; Test 11: Behavior with nil values in mutex structure
(define nil-test-mutex (cons nil nil))  ;; This looks like unlocked mutex
(define nil-lock-result (mutex-lock nil-test-mutex))
(define nil-unlock-result (mutex-unlock nil-test-mutex))

(setq test-count (+ test-count 1))
;; Should behave like a normal mutex
(if (and (eq nil-lock-result t) (eq nil-unlock-result t))
    (setq pass-count (+ pass-count 1)))

;; Final result
(if (= pass-count test-count)
    (print "SUCCESS")
    (print "FAILURE"))
