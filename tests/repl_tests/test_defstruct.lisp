(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(defstruct point (x y))


(define p1 (make-point))
(define r1 (and (eq (point-x p1) nil)
                (eq (point-y p1) nil)))

(debug_test r1 1)

(define p2 (make-point 100 200))
(define r2 (and (= (point-x p2) 100)
                (= (point-y p2) 200)))

(debug_test r2 2)


(defstruct sg1 (daniel sam tealc jack))

(define a (make-sg1 'michael 'amanda 'christopher 'richard))
(define r3 (and (eq (sg1-daniel a) 'michael)
                (eq (sg1-sam a) 'amanda)
                (eq (sg1-tealc a) 'christopher)
                (eq (sg1-jack a) 'richard)))

(debug_test r3 3)


(define b (make-sg1))
(define r4 (and (eq (sg1-daniel b) nil)
                (eq (sg1-sam b) nil)
                (eq (sg1-tealc b)  nil)
                (eq (sg1-jack b)nil)))

(debug_test r4 4)


;; Setters via accessor functions (extra arg -> setix instead of ix)
(define p3 (make-point 1 2))
(point-x p3 42)
(define r5 (and (= (point-x p3) 42)
                (= (point-y p3) 2)))

(debug_test r5 5)

(point-y p3 99)
(define r6 (and (= (point-x p3) 42)
                (= (point-y p3) 99)))

(debug_test r6 6)


;; Generated predicate function
(define r7 (and (point? p1)
                 (point? p3)
                 (not (point? a))
                 (not (point? 5))
                 (not (point? nil))
                 (sg1? a)
                 (not (sg1? p1))))

(debug_test r7 7)


;; Partial initialization (fewer initials than fields)
(define p4 (make-point 100))
(define r8 (and (= (point-x p4) 100)
                (eq (point-y p4) nil)))

(debug_test r8 8)


;; Excess initialization args are ignored
(define p5 (make-point 1 2 3))
(define r9 (and (= (point-x p5) 1)
                (= (point-y p5) 2)))

(debug_test r9 9)


(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9)
    (print "SUCCESS")
    (print "FAILURE"))

              
               
    
