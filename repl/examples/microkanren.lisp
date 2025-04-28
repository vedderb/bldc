
(defun assp (p al)
  (if (eq al nil) nil
    (if (p (car (car al)) (cdr (car al)))
        (assp (p (cdr al))))))

(defun kan-var (c) (list-to-array (list c)))
(defun kan-var? (x) (array? x))
(defun kan-var=? (x1 x2) (= (ix x1 0) (ix x2 0)))

(defun walk (u s)
  (let ((pr (and (kan-var? u) (assp (lambda (v) (kan-var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(defun extend-s (x v s) `((,x . ,v) . ,s))

(defun kan== (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(defun unit (s/c) (cons s/c mzero))
(define mzero '())

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (kan-var? u) (kan-var? v) (kan-var=? u v)) s)
      ((kan-var? u) (extend-s u v s))
      ((kan-var? v) (extend-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (kan-var c)) `(,(car s/c) . ,(+ c 1))))))

(defun disj (g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(defun conj (g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(defun mplus (a b)
  (match a
         (nil b)
         ((closure _ _ _ _) lambda () (mplus b (a)))
         (_ (cons (car a) (mplus (cdr a) b)))))

(defun bind (a g)
  (match a
         (nil mzero)
         ((closure _ _ _ _) (lambda () bind (a) g))
         (_ (mplus (g (car a)) (bind (cdr a) g)))))


(define empty-state '(() . 0))

;; (let ((a ((call/fresh (lambda (q) (kan== q 5))) empty-state)))
;;   (car a))

;;'(((#(0) . 5)) . 1))

(define a-and-b
  (conj 
   (call/fresh (lambda (a) (kan== a 7)))
   (call/fresh 
    (lambda (b) 
      (disj
       (kan== b 5)
       (kan== b 6))))))

(define love
  (call/fresh (lambda (res)
                (call/fresh (lambda (a)
                              (call/fresh (lambda (b)
                                            (call/fresh (lambda (c)
                                                          (conj (kan== a 'i)
                                                                (conj 
                                                                 (kan== b 'love)
                                                                 (conj 
                                                                  (kan== c 'you)
                                                                  (kan== res (list a b c))))))))))))))

(define var-subst (lambda (ls as)
                    (if (eq nil ls ) nil
                      (let (( a (car ls))
                            ( b (assoc as a)))
                        (if b (cons b (var-subst (cdr ls) as))
                          (cons a (var-subst (cdr ls) as)))))))
                    
(cdr (let ((res (love empty-state))) (var-subst (car (car (car res))) (cdr (car (car res))))))

;; (test-check "second-set t1"
;;   (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
;;     (car $))
;;   '(((#(0) . 5)) . 1))
