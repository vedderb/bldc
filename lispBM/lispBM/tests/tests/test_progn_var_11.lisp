
(progn
  (var f (lambda (x)
           (if (= 0 x) 0
             (+ x (f (- x 1))))))
           
  (var a 10)
  (check ( = (f a) 55))
  )
