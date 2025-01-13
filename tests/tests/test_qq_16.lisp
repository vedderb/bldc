(define lamlet
  (macro (bindings body) {
         (var vars (map (lambda (x) (car x)) bindings))
         (var exps (map (lambda (x) (car (cdr x))) bindings))
         `((lambda ,vars ,body) ,@exps)
         }))
                      
(check (= (lamlet ((x 10)) (+ x 1)) 11))
