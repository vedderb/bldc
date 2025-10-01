(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

;; Load a program file once
(define file-handle (fopen "repl_tests/test_data/program_1000.lisp" "r"))
(define test-program (load-file file-handle))

(define expected-result 120)

(define expected-bytes (length test-program))

;; Stress test: run read-eval-program repeatedly in a loop
(define stress-iterations 1000)
(define failures 0)

(define stress-test 
  (lambda (iteration)
    (if (< iteration stress-iterations)
        {

        (define tp (load-file file-handle)) ;; replace over and over again
        
        (if (= (length tp) expected-bytes)
               {
               (if (= (mod iteration 10) 0) 
                   (print "Iteration " iteration " - OK"))
               }
               {
               (print "FAILURE at iteration " iteration ": got " result " expected " expected-result)
               (setq failures (+ failures 1))
               })
            (stress-test (+ iteration 1))
            }
        (print "Stress test complete. Iterations: " iteration " Failures: " failures))))

;; Run the stress test
(stress-test 0)

(fclose file-handle)

;; Final verification
(define final-result (read-eval-program tp))
(define r1 (= final-result expected-result))

(debug_test r1 1)

(if (and r1 (= failures 0))
    (print "SUCCESS")
    (print "FAILURE"))
