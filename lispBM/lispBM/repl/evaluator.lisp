
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

(defun done (e)
  e)

(defun eval-progn (env args k)
  (match args
         (nil (k nil))
         (((? l) . nil) (evalk env l k))
         (((? l) . (? ls))
          (evalk env l
                 (lambda (x)
                   (eval-progn env ls k)))))
  )

(defun eval-define (env args k)
  (let ((key (car args))
        (val (car (cdr args))))
    (evalk env val
           (lambda (x)
             (progn
               (setvar 'global-env
                       (acons key x global-env))
               (k x))))))

(defun eval-lambda (env args k)
  (k (append (cons 'closure args) (list env))))

(defun eval-if (env args k)
  (let ((cond-exp  (car args))
        (then-branch (car (cdr args)))
        (else-branch (car (cdr (cdr args)))))
    (evalk env cond-exp
           (lambda (x) (if x
                           (evalk env then-branch k)
                           (evalk env else-branch k))))))

(defun eval-list (env ls acc k)
  (if (eq ls nil)
      (k acc)
    (let (( l (car ls))
          ( r (cdr ls)))
      (evalk env l
             (lambda (x)
               (eval-list env r (append acc (list x)) k))))))

(defun apply-closure (env ls k)
  (let ((clo  (car ls))
        (args (cdr ls))
        (ps (car (cdr clo)))
        (body (car (cdr (cdr clo))))
        (env1 (car (cdr (cdr (cdr clo)))))
        (arg-env (zip ps args))
        (new-env (add-bindings (append env1 env) arg-env)))
    (evalk new-env body k)))

(defun apply (env ls k)
  (let ((f (car ls)))
    (if (is-operator f)
        (k (eval ls))
      (if (is-closure f)
          (apply-closure env ls k)
        'error))))

(defun evalk (env exp k)
  (if (is-operator exp)
      (k exp)
    (if (is-symbol exp)
        (let ((res (assoc env exp)))
          (if (eq res nil)
              (k (assoc global-env exp))
            (k res)))
      (if (is-number exp)
          (k exp)
        (match exp
               ((progn  . (? ls)) (eval-progn  env ls k))
               ((define . (? ls)) (eval-define env ls k))
               ((lambda . (? ls)) (eval-lambda env ls k))
               ((if . (? ls))     (eval-if env ls k))
               ((?cons ls)        (eval-list env ls nil
                                             (lambda
                                               (rs)
                                               (apply env rs k))))
               )))))

(define test1 '(define apa 1))

(define test2 '(progn (define apa 1) (define bepa 2) (define cepa 3)))

(define test3 '((lambda (x) (+ x 10)) 1))

(define test4 '(progn
		 (define f (lambda (x)
			     (if (= x 0)
				 0
			       (f (- x 1)))))
		 (f 10)))

(define test5 '(progn
		 (define g (lambda (acc x)
			     (if (= x 0)
				 acc
			       (g (+ acc x)
				  (- x 1)))))
		 (g 0 10)))

(define test6 '(progn (define f (lambda (x) (+ x 10)))
                      (define g (lambda (x) (* x 5)))
                      (f (g 10))))

(define test7 '(progn (define f (lambda (x) (+ x 10)))
                      (define g (lambda (x) (* x 5)))
                      (g (f 10))))

(define test8 '((lambda (x) ((lambda (x) (+ x 1)) 7)) 1))

(define test9 '(+ (define apa 1) 2))

