;; Lisp in small pieces chapter 2 page 58.
;; Variations on let.

(defun macro? (x)
   (and (eq (type-of x) type-list)
        (eq (car x) 'macro)))
      

(defun macro-param-list (x)
  (ix x 1))

(defun macro-body (x)
  (ix x 2))

(defun zip (ls1 ls2)
  (match ls1
         (((? a) . (? as)) (cons (list a (car ls2)) (zip as (cdr ls2))))
         (_ nil)))

(defun macro-expand (ma)
  (let (((a . as) ma)
        (evala (eval a)))
      (if (macro? evala) {
        (var params (macro-param-list evala))
        (var env (zip params as))
        (eval env (macro-body evala))
        }
        nil)))


;; let can be implemented using lambda.
;; letrec can be implemented using let.
;;
;; LBM only has letrec and we call it let.

;; simulate let as implemented using lambda
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
