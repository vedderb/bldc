


(define fib (lambda (n)
              (if (< n 2) 1
                (+ (fib (- n 2)) (fib (- n 1)) 1))))






(def num (fib 20))

(def start (time))

(loop (( a 0 )) (< a 1000)
      {
      (fib 20)
      (setq a (+ a 1))
      }
      )

(def end (time))

(print (/ ( * 1000 num) (/ (- end start) 1000000.0)) " fibs /  seconds")

