
(define defun (macro (name param body)
                     `(define ,name (lambda ,param ,body))))


(define defunref (macro (name param body)
                        `(define ,name (lambda ,param (call-cc-unsafe (lambda (return) ,body))))))

                        
