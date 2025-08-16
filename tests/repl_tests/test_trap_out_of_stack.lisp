
(define ok1 nil)
(define ok2 nil)

(defun f (n) (if (= n 0) 0 (+ n (f (- n 1 )))))

(defun g () {
       (print "about to overflow the stack")
        (let ((a (trap (f 100))))
          {
          (print "hello")
          (print a)
          (print "world")       
          
          })
        (print "is the stack still ok?")
        (setq ok1 t)
        })

(g)


(defun h (n a b c d e f g)
  (if (= n 0) 0
      (+ a b c d e f g (h (- n 1) a b c d e f g h))))

(defun i () {
       (print "about to overflow the stack")
        (let ((a (trap (h 100 1 2 3 4 5 6 7 ))))
          {
          (print "hello")
          (print a)
          (print "world")       
          
          })
        (print "is the stack still ok?")
        (setq ok2 t)
        })

(i)

(if (and ok1 ok2)
    (print "SUCCESS")
    (print "FAILURE"))
