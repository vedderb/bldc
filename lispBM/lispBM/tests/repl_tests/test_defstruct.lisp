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


(if (and r1 r2 r3 r4)
    (print "SUCCESS")
    (print "FAILURE"))

              
               
    
