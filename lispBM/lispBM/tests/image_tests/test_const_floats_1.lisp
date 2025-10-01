
(def r 0)

(def my-f1 3.14)
(def my-fl1 (list 3.14 6.28 100.0))

(def t1 (list 1 2 3))
(def t2 (list t1 t1))
(def t3 (list t2 t2))

@const-start

(def ct1 (list 1 2 3))
(def ct2 (list ct1 ct1))
(def ct3 (list ct2 ct2))

(define feq (lambda (a b epsilon)
              (< (abs (- a b)) epsilon)))

(def my-f2 3.14)
(def my-fl2 (list 3.14 6.28 100.0))

(defun f () {
  (looprange i 0 100 {
        ;;(print i)
        (setq r i)
        })
  r
  })

(defun main () {
  (if (and (= (f) 99)
           (feq my-f1 3.14 0.001)
           (feq my-f2 3.14 0.001)
           (feq (ix my-fl1 2) 100.0 0.001)
           (feq (ix my-fl2 2) 100.0 0.001))
           
      
      (print "SUCCESS")
      (print "FAILURE")
      )
  (print t3)
  (print ct3)
  })

@const-end

(print "a " t3)
(print "a " ct3)
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
