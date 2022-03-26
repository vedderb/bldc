
(define fred (lambda ()
               (progn (print "fred iteration" \#newline )
                      (recv ( (apa (? x) 107)  (print "fred received apa " x \#newline))
                            ( (bepa (?i x))  (print "fred received bepa " x \#newline)))
                      (fred))))

(let ((apa 1000))
  (define bella (lambda (pid x)
                  (progn (print "bella " apa " iteration" x \#newline)
                         (send pid `(apa ,x 107))
                         (yield 500000)
                         (print "bella waking up" \#newline)
                         (send pid '(bepa 2))
                         (yield 500000)
                         (bella pid (+ x 1)))))
  )

(let ((silly-greeter
       (lambda ()
         (progn (print "GREETINGS!" \#newline)
                (yield 2500000)
                (silly-greeter)))))
  (spawn silly-greeter))


(spawn (let ((mogwai
              (lambda ()
                (progn (print "GIZMO!" \#newline)
                       (yield 2500000)
                       (mogwai)))))
         mogwai))

(spawn (let ((space
              (lambda ()
                (progn (print \#newline \#newline)
                       (yield 2500000)
                       (space)))))
         space))


(define f (lambda (x y z)
            (+ x y z)))

(spawn 20 f 1 2 3)

(define fredpid (spawn fred))

(spawn bella fredpid 0)

