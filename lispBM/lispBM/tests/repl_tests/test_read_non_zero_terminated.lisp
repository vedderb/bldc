

(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(define time-read-eval
    (macro (str)
           `(progn
              (var t0 (systime))
              (var res (read-eval-program ,str))
              (print (- (systime) t0))
              res)))


;; Test 1
(define s1 "(+ 1 2) (+ 3 4)") ;; length 16

;; Destroy the 0 termination
(bufset-u8 s1 15 \#a)

(define r1 (eq 'exit-error (car (trap (read-eval-program s1)))))

(debug_test r1 1)

;; Test 2
(define s2 "danieljacksontealcsamjackaaaaaa") ;; length 32

(print (length s2))

;; Destroy the 0 termination
(print s2)
(bufset-u8 s2 31 \#a)
(print s2)

(define r2 (eq 'exit-error (car (trap (read-eval-program s2)))))

(debug_test r2 2)

;; Test 3
(define s3 "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb") ;; length 64

(print (length s3))

;; Destroy the 0 termination
(print s3)
(bufset-u8 s3 63 \#a)
(print s3)

(define r3 (eq 'exit-error (car (trap (read-eval-program s3)))))

(debug_test r3 3)


;; Test 4: Very small buffer (1 byte)
(define s4 "a")
(bufset-u8 s4 0 65) ;; Set to 'A' without null termination
(define r4 (eq 'exit-error (car (trap (read-eval-program s4)))))
(debug_test r4 4)

;; Test 5: Empty-like buffer (just non-null byte)
(define s5 "x")
(bufset-u8 s5 0 88) ;; Set to 'X' without null termination
(define r5 (eq 'exit-error (car (trap (read-eval-program s5)))))
(debug_test r5 5)

;; Test 6: Test with read-program (not just read-eval-program)
(define s6 "(+ 1 2)")
(bufset-u8 s6 6 65) ;; Replace null with 'A'
(define r6 (eq 'exit-error (car (trap (read-program s6)))))
(debug_test r6 6)

;; Test 7: Test with read function
(define s7 "42")
(bufset-u8 s7 2 66) ;; Replace null with 'B'
(define r7 (= 42 (read s7)))
(debug_test r7 7)

;; Test 8: Byte array approach (more direct vulnerability trigger)
(define ba1 [40 43 32 49 32 50 41]) ;; "(+ 1 2)" without null termination
(trap (read-eval-program ba1)) ;; May be error or OK depending on what it reads into when it overflows
(define r8 t)  ;; it doesnt crash.
(debug_test r8 8) 

;; Test 9: Large byte array to increase chance of overread
(define ba2 [65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 
             65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65
             65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65
             65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65]) ;; 64 'A's
(define r9 (eq 'exit-error (car (trap (read-eval-program ba2)))))
(debug_test r9 9)

;; Test 10: Byte array with valid Lisp syntax but no termination
(define ba3 [40 43 32 49 32 50 41 32 40 43 32 51 32 52 41]) ;; "(+ 1 2) (+ 3 4)"
(read-eval-program ba3)
(define r10 t)
(debug_test r10 10)

;; Using these tests we can witness how the reader overruns the buffer in case
;; it is not properly zero-terminated.
;; The tests do not seem to trigger a crash but that is likely just a matter of luck.

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10)
    (print "SUCCESS")
    (print "FAILURE"))





