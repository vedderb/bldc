
(define forever (lambda () {
                  (sleep 1)
                  (print "Hello banana world")
                  (forever)
                  }))
                  

(spawn forever)
