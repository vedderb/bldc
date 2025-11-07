

(define arr [| 1 2 3 4 5 |])

(setix arr 0 '(apa bepa cepa))
(setix arr -2 '(kurt russel))

(define e1 (trap (setix arr 'apa 10)))
(define e2 (trap (setix arr 10)))
(define e3 (trap (setix arr 0 'apa 100)))


(if (and (eq (ix arr -5) '(apa bepa cepa))
         (eq (ix arr 3)  '(kurt russel))
         (eq '(exit-error type_error) e1)
         (eq '(exit-error type_error) e2)
         (eq '(exit-error type_error) e3))
          
    (print "SUCCESS")
    (print "FAILURE"))
