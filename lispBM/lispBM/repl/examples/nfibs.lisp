


(def fib (lambda (n)
           (if (< n 2) 1
             (+ (fib (- n 2)) (fib (- n 1)) 1))))





(def num (fib 20))

(def start (systime))

(loop (( a 0 )) (< a 1000)
      {
      (fib 20)
      (setq a (+ a 1))
      }
      )

(def t_tot (secs-since start))

(print (/ ( * 1000 num) t_tot) " fibs /  seconds")

