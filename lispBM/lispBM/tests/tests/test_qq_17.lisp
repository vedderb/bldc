(define lamlet
  (macro (bindings body) {
         (var vars (map (lambda (x) (car x)) bindings))
         (var exps (map (lambda (x) (car (cdr x))) bindings))
         `((lambda ,vars ,body) ,@exps)
         }))



(define lamletrec
  (macro (bindings body) {
         (var nilbinds (map (lambda (x) (list (car x) nil)) bindings))
         (var expassigns (map (lambda (x) (list 'setq (car x) (car (cdr x)))) bindings))
         `(lamlet ,nilbinds
                  {
                  ,@expassigns
                  ,body
                  })
         }))

(define test_1 (lamletrec ((f (lambda (x) (+ (g x) 1)))
                           (g (lambda (x) (+ x 1))))
                          (f 5)))

(check (= test_1 7))
