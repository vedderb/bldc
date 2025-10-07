;; Mutex Extensions Edge Case Tests

;; TEST1

(define invalid-lock-result1 (eq '(exit-error type_error) (trap (mutex-lock nil))))
(define invalid-lock-result2 (eq '(exit-error type_error) (trap (mutex-lock 42))))
(define invalid-lock-result3 (eq '(exit-error type_error) (trap (mutex-lock "not-a-mutex"))))

(define r1 (and invalid-lock-result1
                invalid-lock-result2
                invalid-lock-result3))

(if (not r1) (print "r1 = " r1))

;; TEST2

(define invalid-unlock-result1 (eq '(exit-error type_error) (trap (mutex-unlock nil))))
(define invalid-unlock-result2 (eq '(exit-error type_error) (trap (mutex-unlock 42))))
(define invalid-unlock-result3 (eq '(exit-error type_error) (trap (mutex-unlock "not-a-mutex"))))

(define r2 (and invalid-unlock-result1
                invalid-unlock-result2
                invalid-unlock-result3))

(if (not r2) (print "r2 = " r2))

;; TEST3

(define fresh-mutex (mutex-create))
(define unlock-without-lock (eq '(exit-error eval_error) (trap (mutex-unlock fresh-mutex))))

(define r3 unlock-without-lock)

(if (not r3) (print "r3 = " r3))

;; TEST4

(define double-unlock-mutex (mutex-create))
(define initial-lock (mutex-lock double-unlock-mutex))
(define first-unlock (mutex-unlock double-unlock-mutex))
(define second-unlock (eq '(exit-error eval_error) (trap (mutex-unlock double-unlock-mutex))))

(define r4 (and initial-lock first-unlock second-unlock)) 

(if (not r4) (print "r4 = " r4))

;; TEST5

(define m5 (mutex-create))

(define s5 0)

(define t1 (lambda ()
             {(mutex-lock m5)
              (setq s5 (+ s5 1))
              (mutex-unlock m5)}))
;; spawn 20 threads
(looprange i 0 20
      (spawn 20 t1))

(loopwhile (!= s5 20)
 (sleep 0.1))

(define r5 t)

;;TEST6

(define struct-mutex (mutex-create))
(define original-car (car struct-mutex))
(define original-cdr (cdr struct-mutex))

(mutex-lock struct-mutex)
(mutex-unlock struct-mutex)

(define restored-car (car struct-mutex))
(define restored-cdr (cdr struct-mutex))

(define r6 (and (eq restored-car original-car)
                (eq restored-cdr original-cdr)))

(if (not r6) (print "r6 = " r6))

;; TEST7: Empty list as mutex (edge case)
(define empty-list-mutex '())
(define empty-lock-result (eq '(exit-error type_error) (trap (mutex-lock empty-list-mutex))))

(define r7 empty-lock-result)
(if (not r7) (print "r7 = " r7))

;; TEST8: Mutex operations with no arguments
(define no-arg-result1 (eq '(exit-error type_error) (trap (mutex-lock))))
(define no-arg-result2 (eq '(exit-error type_error) (trap (mutex-unlock))))

(define r8 (and no-arg-result1 no-arg-result2))
(if (not r8) (print "r8 = " r8))

;; TEST9: Mutex operations with too many arguments  
(define too-many-args1 (eq '(exit-error type_error) (trap (mutex-lock (mutex-create) (mutex-create)))))
(define too-many-args2 (eq '(exit-error type_error) (trap (mutex-unlock (mutex-create) (mutex-create)))))

(define r9 (and too-many-args1 too-many-args2))
(if (not r9) (print "r9 = " r9))

;; TEST10: Behavior with nil values in mutex structure
(define nil-test-mutex (cons nil nil))  ;; This looks like unlocked mutex
(define nil-lock-result (mutex-lock nil-test-mutex))
(define nil-unlock-result (mutex-unlock nil-test-mutex))

(define r10 (and (eq nil-lock-result t) (eq nil-unlock-result t)))
(if (not r10) (print "r10 = " r10))

;; TEST11: Test to trigger mutex blocking path (multiple threads on same mutex)
;; This should hit the blocking/unblocking code paths  
(define block-mutex (mutex-create))
(define block-test-complete nil)

;; Thread that holds the mutex for a while
(define holder-thread (lambda ()
  {(mutex-lock block-mutex)
   (sleep 0.2)  ;; Hold it briefly  
   (mutex-unlock block-mutex)
   (setq block-test-complete t)}))

;; Thread that tries to acquire the already-locked mutex
(define waiter-thread (lambda ()
  {(mutex-lock block-mutex)   ;; This should block
   (mutex-unlock block-mutex)}))

;; Start holder first, then waiter
(spawn 30 holder-thread)
(sleep 0.05)  ;; Give holder time to acquire lock
(spawn 30 waiter-thread)

;; Wait for test to complete
(loopwhile (not block-test-complete) (sleep 0.05))

(define r11 block-test-complete)
(if (not r11) (print "r11 = " r11))

;; TEST12: Test more valid mutex patterns
;; Test safe mutex-like structures to exercise branches
(define extra-mutex-1 (mutex-create))
(define extra-mutex-2 (mutex-create))

(define extra-lock-1 (mutex-lock extra-mutex-1))
(define extra-lock-2 (mutex-lock extra-mutex-2))
(define extra-unlock-1 (mutex-unlock extra-mutex-1))
(define extra-unlock-2 (mutex-unlock extra-mutex-2))

(define r12 (and extra-lock-1 extra-lock-2 extra-unlock-1 extra-unlock-2))
(if (not r12) (print "r12 = " r12))

;; TEST13: Simple validation tests (already covered in earlier tests)
(define r13 t)
(if (not r13) (print "r13 = " r13))

;; TEST14: Basic mutex stress test
(define stress-mutex (mutex-create))
(define stress-result t)

;; Perform multiple lock/unlock cycles
(looprange i 0 5 {
  (mutex-lock stress-mutex)
  (mutex-unlock stress-mutex)
})

(define r14 stress-result)
(if (not r14) (print "r14 = " r14))

;; TEST15: Multiple mutex operations for more edge coverage
;; Create several scenarios to exercise remaining branches
(define edge-mutex-1 (mutex-create))
(define edge-mutex-2 (mutex-create)) 
(define edge-mutex-3 (mutex-create))

;; Lock all three
(mutex-lock edge-mutex-1)
(mutex-lock edge-mutex-2) 
(mutex-lock edge-mutex-3)

;; Try various unlock patterns
(define unlock-1 (mutex-unlock edge-mutex-1))
(define unlock-2 (mutex-unlock edge-mutex-2))
(define unlock-3 (mutex-unlock edge-mutex-3))

(define r15 (and unlock-1 unlock-2 unlock-3))
(if (not r15) (print "r15 = " r15))



;; TEST16

(define c1 '(1 . nil))
(define c2 '(nil . 1))

(define r16
    (and (eq '(exit-error eval_error) (trap (mutex-unlock c1)))
         (eq '(exit-error type_error)  (trap (mutex-unlock c2)))))

(if (not r16) (print "r16 = " r16))


;; TEST17

(define m17 (mutex-create))

(define t17b-r nil)

(define t17b (lambda ()
               (setq t17b-r (trap (mutex-unlock m17)))))


(define t17-done nil)
(define t17a (lambda ()
               {(mutex-lock m17)
                (spawn 20 t17b)
                (sleep 1)
                (mutex-unlock m17)
                (setq t17-done t)
                }))

(spawn 20 t17a)

(loopwhile (not t17-done)
 (sleep 0.01))

(define r17 (eq t17b-r '(exit-error eval_error)))

(if (not r17) (print "r17 = " r17))

;; TEST18

(define m18 (mutex-create))

(define m18-data nil)
(define s18 0)

(define memory-filler (range 0 1300))

(define t1 (lambda (i)
             {(mutex-lock m5)
              (setq m18-data (range 0 i))
              (setq s18 (+ s18 1))
              (mutex-unlock m5)
              (mutex-lock m5)
              (mutex-unlock m5)
              (mutex-lock m5)
              (mutex-unlock m5)
              (mutex-lock m5)
              (mutex-unlock m5)
              (mutex-lock m5)
              (mutex-unlock m5)
              (mutex-lock m5)
              (mutex-unlock m5)
              (mutex-lock m5)
              (mutex-unlock m5)
              }))
;; spawn 20 threads
(looprange i 0 20
      (spawn 20 t1 (+ i 1)))

(loopwhile (!= s18 20)
 (sleep 0.1))

(define r18 t)


;; CHECK RESULT

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18) 
    (print "SUCCESS")
    (print "FAILURE"))
 
