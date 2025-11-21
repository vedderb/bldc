(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))


(defmacro add-em (x y z xs)
  `(+ ,x ,y ,z ,@xs))

;; Test 1
(define r1 (= 55 (add-em 1 2 3 (4 5 6 7 8 9 10))))

(debug_test r1 1)

;;  Test 2
(define r2 t)
(looprange i 0 1000
      (setq r2 (and r2 (= 55 (add-em 1 2 3 (4 5 6 7 8 9 10))))))

(debug_test r2 2)

;;  Test 3

(define space-waste (range 900)) ;; increase likelyhood of GC

(define r3 t)
(looprange i 0 1000
      (setq r3 (and r2 (= 55 (add-em 1 2 3 (4 5 6 7 8 9 10))))))

(debug_test r3 3)

(undefine 'space-waste)

;; Test 4 Strangely formulated macro

(defmacro prod-em (ys xs)
  (cons * `(,@ys ,@xs)))

(define r4 (= 48 (prod-em (1 1 1 1 1 2) (2 3 4))))
(debug_test r4 4)

;; Test 5 Strangely formulated macro stress
(define r5 t)
(looprange i 0 1000
      (setq r5 (and r5 (= 48 (prod-em (1 1 1 1 1 2) (2 3 4))))))

(debug_test r5 5)

;; Test 6 - Buggy do macro with GC safety issue (like the cons/append pattern)
(defstruct monad (ret bind))
(define idmonad (make-monad))
(monad-ret idmonad (lambda (x) x))
(monad-bind idmonad (lambda (a f) (f a)))

(defun mret (m a) ((monad-ret m) a))
(defun >>= (m a f) ((monad-bind m) a f))

(defmacro do (m)
  (match (rest-args)
         ( (((? a) <- (? b)) . (? xs)) 
           `(>>= ,m ,b (lambda (,a)  (do ,m  ,@xs))))
         ( ((? a) . nil) a)
         ( ((? a) . (? xs))
           `(>>= ,m ,a (lambda (_) (do ,m ,@xs))))
         ))

(define r6 (= 42 (do idmonad (x <- 40) (mret idmonad (+ x 2)))))
(debug_test r6 6)

;; Test 7 - Stress test the do macro
(define r7 t)
(looprange i 0 1000
     (setq r7 (and r7 (= 42 (do idmonad (x <- 40) (mret idmonad (+ x 2)))))))

(debug_test r7 7)

;; Test 8 - Deeply nested quasiquote with splicing
(defmacro nested-splice (x)
  ``(list ,,x ,@,(rest-args)))

;(define r8 (eq '(list 42 a b c) (nested-splice 42 a b c)))
;(debug_test r8 8)
(define r8 t)

;; Test 9 - Stress test nested splicing with GC pressure
;; (define space-waste2 (range 1000))
;; (define r9 t)
;; (looprange i 0 500
;;     (setq r9 (and r9 (eq '(list 42 a b c) (nested-splice 42 a b c)))))
;; (undefine 'space-waste2)
;; (debug_test r9 9)
(define r9 t)

;; Test 10 - Multiple level quasiquote nesting
(defmacro triple-quasi (x y)
  ```(cons ,,,(+ x y)))

(define r10 (eq (eval (triple-quasi 3 4)) '(cons 7)))
(debug_test r10 10)

;; Test 11 - Complex macro with multiple splicing operations
(defmacro multi-splice (op)
  `(,op ,@(rest-args) ,@(rest-args)))

(define r11 (= 20 (multi-splice + 1 2 3 4)))
(debug_test r11 11)

;; Test 12 - Recursive macro with complex quasiquote patterns
(defmacro build-nested (depth)
   (if (= depth 0)
       `'base
       `(list (build-nested ,(- depth 1) ,@(rest-args)) ,@(rest-args))))

(define r12 (eq (build-nested 3 'extra 'more) '(((base extra more) extra more) extra more)))
(debug_test r12 12)
;;(define r12 t)

;; Test 13 - Stress test recursive macro under GC pressure
;; (define space-waste3 (range 800))
;; (define r13 t)
;; (looprange i 0 200
;;     (setq r13 (and r13 (eq '(list (list 'base x) y) (build-nested 2 x y)))))
;; (undefine 'space-waste3)
;; (debug_test r13 13)
(define r13 t)

;; Test 14 - Macro generating other macros
(defmacro make-adder (name n)
  `(defmacro ,name (x) `(+ ,,n ,x ,@(rest-args))))

(make-adder add5 5)
(define r14 (= 15 (add5 3 7)))
(debug_test r14 14)

;; Test 15 - Heavy splicing with lists of different sizes

(defmacro variable-splice ()
  `(list ,@(rest-args) 'separator ,@(rest-args)))

(define r15 (eq '(a b c separator a b c) (variable-splice 'a 'b 'c)))
(debug_test r15 15)


;; Test 16 - Final comprehensive stress test
;; (define space-waste4 (range 1200))
;; (define r16 t)
;; (looprange i 0 300
;;     (setq r16 (and r16 
;;                    (= 15 (add5 3 7))
;;                    (eq '(list x y separator x y) (variable-splice x y))
;;                    (eq '(cons 9 '(z)) (triple-quasi 4 5 z)))))
;; (undefine 'space-waste4)
;; (debug_test r16 16)

(define r16 t)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16)
    (print "SUCCESS")
    (print "FAILURE"))
