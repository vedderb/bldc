
(define kurt (lambda ()
               (progn
                 (atomic
                  (progn
                    (print "hello world")
                    (print "hoho")
                    (print "haha")
                    (print "apa")
                    (print "bepa")
                    (print "cepa")
                    )
                  )
                 (yield 1000000)
                 (kurt)
                 )
               )
  )


(define fred (lambda ()
               (progn
                 (print "FRED!")
                 (print "fred iteration\n" )
                 (recv ( (apa (? x) 107) (print "fred received apa " x "\n"))
                       ( (bepa (? x))  (print "fred received bepa " x "\n")))
                 (fred))))

(let ((apa 1000))
  (define bella (lambda (pid x)
                  (progn (print "bella " apa " iteration" x "\n")
                         (send pid `(apa ,x 107))
                         (yield 500000)
                         (print "bella waking up\n")
                         (send pid '(bepa 2))
                         (yield 500000)
                         (bella pid (+ x 1)))))
  )

;; (let ((silly-greeter
;;        (lambda ()
;;          (progn (print "GREETINGS!" \#newline)
;;                 (yield 2500000)
;;                 (silly-greeter)))))
;;   (spawn silly-greeter))


;; (spawn (let ((mogwai
;;               (lambda ()
;;                 (progn (print "GIZMO!" \#newline)
;;                        (yield 2500000)
;;                        (mogwai)))))
;;          mogwai))

;; (spawn (let ((space
;;               (lambda ()
;;                 (progn (print \#newline \#newline)
;;                        (yield 2500000)
;;                        (space)))))
;;          space))

;;(spawn 20 f 1 2 3)

(define fredpid (spawn fred))

(spawn bella fredpid 0)

