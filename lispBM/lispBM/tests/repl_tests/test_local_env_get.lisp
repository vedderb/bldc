


(define lenv
    (let ((a 10)
          (b 20)
          (c 30))
      (local-env-get)))


(if (and (= (assoc lenv 'a) 10)
         (= (assoc lenv 'b) 20)
         (= (assoc lenv 'c) 30))
    (print "SUCCESS")
  (print "FAILURE"))
