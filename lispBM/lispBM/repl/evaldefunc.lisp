
(define global-env 'nil)

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

(defun eval-progn (env args k)
  (match args
         (nil (apply-cont k nil))
         (((? l) . nil) (evald env l k))
         (((? l) . (? ls))
          (evald env l
                 (list 'progn-cont env ls k)))))

(defun eval-define (env args k)
  (let ((key (car args))
        (val (car (cdr args))))
    (evald env val
           (list 'define-cont key k))))

(defun eval-lambda (env args k)
  (apply-cont k (append (cons 'closure args) (list env))))

(defun eval-if (env args k)
  (let ((cond-exp  (car args))
        (then-branch (car (cdr args)))
        (else-branch (car (cdr (cdr args)))))
    (evald env cond-exp
           (list 'if-cont env then-branch else-branch k))))

(defun eval-list (env ls acc k)
  (if (eq ls nil)
      (apply-cont k acc)
      (let (( l (car ls))
            ( r (cdr ls)))
        (evald env l
               (list 'list-cont env r acc k)))))

(defun apply-closure (env ls k)
  (let ((clo  (car ls))
        (args (cdr ls))
        (ps (car (cdr clo)))
        (body (car (cdr (cdr clo))))
        (env1 (car (cdr (cdr (cdr clo)))))
        (arg-env (zip ps args))
        (new-env (add-bindings (append env1 env) arg-env)))
    (evald new-env body k)))

(defun apply (env ls k)
   (let ((f (car ls)))
     (if (is-operator f)
         (apply-cont k (eval ls))
         (if (is-closure f)
             (apply-closure env ls k)
             'error))))

(defun apply-cont (k exp)
  (match k
         (done exp)
         ((progn-cont (? env) (? ls) (? k1)) (eval-progn env ls k1))
         ((define-cont (? key) (? k1))
          (progn
            (setvar 'global-env (acons key exp global-env))
            (apply-cont k1 exp)))
         ((list-cont (? env) (? r) (? acc) (? k1))
          (eval-list env r (append acc (list exp)) k1))
         ((application-cont (? env) (? k1))
          (apply env exp k1))
         ((if-cont (? env) (? then-branch) (? else-branch) (? k1))
          (if exp
              (evald env then-branch k1)
            (evald env else-branch k1)))))

(defun evald (env exp k)
  (if (is-operator exp)
      (apply-cont k exp)
    (if (is-symbol exp)
        (let ((res (assoc env exp)))
          (if (eq res nil)
              (apply-cont k (assoc global-env exp))
            (apply-cont k res)))
      (if (is-number exp)
          (apply-cont k exp)
        (match exp
               ((progn  . (? ls)) (eval-progn  env ls k))
               ((define . (? ls)) (eval-define env ls k))
               ((lambda . (? ls)) (eval-lambda env ls k))
               ((if . (? ls))     (eval-if env ls k))
               ((?cons ls)        (eval-list env ls nil
                                             (list 'application-cont env k)))
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
