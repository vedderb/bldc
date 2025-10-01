

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
(define r1 (= 7 (time-read-eval "(+ 1 2) (+ 3 4)")))

(debug_test r1 1)

;; Test 2
(define r2 (eq 'exit-error (car (trap (time-read-eval "(undefined-function 42)")))))

(debug_test r2 2)

;; Test 3
(define r3 (eq 'exit-error (car (trap (time-read-eval "(unclosed paren")))))

(debug_test r3 3)

;; Test 4
(define r4 (eq nil (time-read-eval "")))

(debug_test r4 4)

;; Test 5
(define r5 (eq 'exit-error (car (trap (time-read-eval "(/ 1 0)")))))

(debug_test r5 5)

;; Test 6
(define r6 (= 42 (time-read-eval "(define test-var 42) test-var")))

(debug_test r6 6)

;; Test 7 - Larger program with recursion
(define r7 (= 120 (time-read-eval "
  (define fact (lambda (n)
    (if (< n 2)
        1
        (* n (fact (- n 1))))))
  (fact 5)")))

(debug_test r7 7)

;; Test 8 - Program with loops and list operations
(define r8 (= 5050 (time-read-eval "
  (define sum 0)
  (looprange i 1 101
    (setq sum (+ sum i)))
  sum")))

(debug_test r8 8)

;; Test 9 - Program with nested data structures
(define r9 (= 6 (time-read-eval "
  (define nested '((1 2) (3 4) (5 6)))
  (define process (lambda (lst)
    (if (eq lst nil)
        0
        (+ (length (car lst)) (process (cdr lst))))))
  (process nested)")))

(debug_test r9 9)

;; Test 10 - Program with string operations
(define r10 (eq "HELLO WORLD" (time-read-eval "
  (define msg \"hello world\")
  (str-to-upper msg)")))

(debug_test r10 10)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10)
    (print "SUCCESS")
    (print "FAILURE"))
