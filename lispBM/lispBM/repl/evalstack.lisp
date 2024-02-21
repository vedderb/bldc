
(define global-env 'nil)

(define stack 'nil)

(defun push (v)
  (setvar 'stack (cons v stack)))

(defun pop ()
  (let ((r (car stack)))
    (progn
      (setvar 'stack (cdr stack))
      r)))

(defun is-number (e)
  (or (eq (type-of e) type-i)
      (eq (type-of e) type-u)))

(defun is-symbol (e)
  (eq (type-of e) type-symbol))

(defun is-operator (e)
  (or (eq e '+)
      (eq e '-)
      (eq e '=)
      (eq e '*)
      ))

(defun is-closure (e)
  (and (eq (type-of e) type-list)
       (eq (car e) 'closure)))

(defun add-bindings (env binds)
  (match binds
         (nil env)
         (((? b) . (? rs))
          (add-bindings (setassoc env b) rs))))

(defun eval-progn (env args)
  (match args
         (nil (apply-cont nil))
         (((? l) . nil) (eval-stack env l))
         (((? l) . (? ls))
          (progn
            (push (list 'progn-cont env ls))
            (eval-stack env l)))))
              

(defun eval-define (env args)
  (let ((key (car args))
        (val (car (cdr args))))
    (progn
      (push (list 'define-cont key))
      (eval-stack env val))))
           

(defun eval-lambda (env args)
  (apply-cont (append (cons 'closure args) (list env))))

(defun eval-if (env args)
  (let ((cond-exp  (car args))
        (then-branch (car (cdr args)))
        (else-branch (car (cdr (cdr args)))))
    (progn 
      (push (list 'if-cont env then-branch else-branch))
      (eval-stack env cond-exp))))
          

(defun eval-list (env ls acc)
  (if (eq ls nil)
      (apply-cont acc)
      (let (( l (car ls))
            ( r (cdr ls)))
        (progn
          (push (list 'list-cont env r acc))
          (eval-stack env l)))))
               

(defun apply-closure (env ls)
  (let ((clo  (car ls))
        (args (cdr ls))
        (ps (car (cdr clo)))
        (body (car (cdr (cdr clo))))
        (env1 (car (cdr (cdr (cdr clo)))))
        (arg-env (zip ps args))
        (new-env (add-bindings (append env1 env) arg-env)))
    (eval-stack new-env body)))

(defun apply (env ls)
   (let ((f (car ls)))
     (if (is-operator f)
         (apply-cont (eval ls))
         (if (is-closure f)
             (apply-closure env ls)
             'error))))

(defun apply-cont (exp)
  (let (( k (pop)))
    (match k
           (done exp)
           ((progn-cont (? env) (? ls)) (eval-progn env ls))
           ((define-cont (? key))
            (progn
              (setvar 'global-env (acons key exp global-env))
              (apply-cont exp)))
           ((list-cont (? env) (? r) (? acc))
            (eval-list env r (append acc (list exp))))
           ((application-cont (? env))
            (apply env exp))
           ((if-cont (? env) (? then-branch) (? else-branch))
            (if exp
                (eval-stack env then-branch)
                (eval-stack env else-branch))))))

(defun evals (env exp)
  (progn
    (setvar 'stack nil)
    (push 'done)
    (eval-stack env exp)))
  

(defun eval-stack (env exp)
  (if (is-operator exp)
      (apply-cont exp)
    (if (is-symbol exp)
        (let ((res (assoc env exp)))
          (if (eq res nil)
              (apply-cont (assoc global-env exp))
            (apply-cont res)))
      (if (is-number exp)
          (apply-cont exp)
        (match exp
               ((progn  . (? ls)) (eval-progn  env ls))
               ((define . (? ls)) (eval-define env ls))
               ((lambda . (? ls)) (eval-lambda env ls))
               ((if . (? ls))     (eval-if env ls))
               ((?cons ls)        (progn
                                    (push (list 'application-cont env))
                                    (eval-list env ls nil)))
               )))))

(define test1 '(define apa 1))

(define test2 '(progn (define apa 1) (define bepa 2) (define cepa 3)))

(define test3 '((lambda (x) (+ x 10)) 1))

(define test4 '(progn (define f (lambda (x) (if (= x 0) 0 (f (- x 1))))) (f 10)))

(define test5 '(progn (define g (lambda (acc x) (if (= x 0) acc (g (+ acc x) (- x 1))))) (g 0 10)))

(define test6 '(progn (define f (lambda (x) (+ x 10)))
                      (define g (lambda (x) (* x 5)))
                      (f (g 10))))

(define test7 '(progn (define f (lambda (x) (+ x 10)))
                      (define g (lambda (x) (* x 5)))
                      (g (f 10))))
