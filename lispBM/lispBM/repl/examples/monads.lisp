
(defun concatacc (acc ls)
  (if (eq ls nil) acc
    (concatacc (append acc (car ls)) (cdr ls))))

(defun replicate (a n)
  (if (= n 0) nil
    (cons a (replicate a (- n  1)))))

;; List monad

(defun returnlist (a) (list a))
(defun bindlist (la f)
  (concatacc '() (map f la)))

;; Bunnies example

(defun generation (n)
  (lambda (a) (replicate a n)))

(defun test1 () (bindlist (returnlist "bunny") (generation 5)))
(defun test2 () (bindlist (list "bunny" "rabbit") (generation 5)))

;; a monad object

(defstruct monad (ret bind))

(define listmonad (make-monad))
(monad-ret listmonad returnlist)
(monad-bind listmonad bindlist)

;; Generic monad operations

(defun mret (m a)
  ((monad-ret m) a))

(defun >>= (m a f)
  ((monad-bind m) a f)) 
                
;; Bunnies again

(defun test3 () (>>= listmonad (mret listmonad "bunny") (generation 3)))
(defun test4 () (>>= listmonad (list "bunny" "rabbit") (generation 2)))

;; macro
(defmacro do (m body)
  (match body
         ( (((? a) <- (? b)) . (? xs)) 
           `(>>= ,m ,b (lambda (,a) (do ,m ,xs))))
         ( ((? a) . nil) a)
         ( ((? a) . (? xs))
           `(>>= ,m ,a (lambda (_) (do ,m ,xs))))
         ))

(defun test5 ()
  (do listmonad
    (
     (a <- (list 1 2 3 4))
     (b <- (list 5 6 7 8))
     (mret listmonad (* a b))
     )
    ))

;; Generic monad operation

(defun zip-combos (m f ma mb)
  (do m
      (
       (a <- ma)
       (b <- mb)
       (mret m (f a b)))))

(defun test6 ()
  (zip-combos listmonad (lambda (a b) (* a b)) (list 1 2 3 4) (list 5 6 7 8)))


;; Identity monad

(define idmonad (make-monad))
(monad-ret idmonad (lambda (x) x))
(monad-bind idmonad (lambda (a f) (f a)))

(defun test7 ()
  (zip-combos idmonad (lambda (a b) (* a b)) 2 10))

;; Identity monad vs progn

(defun test8 ()
  (do idmonad
      (
       (print "hello")
       (a <- (+ 1 2))
       (print "the result of (+ 1 2) is " a)
       )))

(defun test9 () ;; PROGN is { } in LBM
  {
  (print "hello")
  (var a (+ 1 2))
  (print "the result of (+ 1 2) is " a)
  }
  )


;; state monad

(define statemonad (make-monad))
(monad-ret statemonad (lambda (x) (lambda (s) (cons s x))))

;; (>>=) :: State s a -> (a -> State s b) -> State s b
;; (act1 >>= fact2) s = runState act2 is 
;;     where (iv,is) = runState act1 s
;;           act2 = fact2 iv
(defun runstate (sa s)
  (s sa)
  )

(monad-bind statemonad (lambda (sa f)
                         (lambda (s)
                           (let (((is . iv) (runstate s sa))
                                 (r1 (f iv)))
                             (runstate is r1)))))

(defun get ()
  (lambda (s) (cons s s)))

(defun put (x)
  (lambda (s) (cons x nil)))


(defun test10 ()
  (do statemonad
      (
       (a <- (get))
       (mret statemonad (print "state0 is " a))
       (put 1)
       (a <- (get))
       (mret statemonad (print "state1 is " a))
       (put 10)
       (a <- (get))
       (mret statemonad (print "state2 is " a))
       (put 100)
       (a <- (get))
       (mret statemonad (print "state3 is " a))
       )
      ))


;; lift1 :: (a -> b) -> ma -> mb
(defun lift1 (m f)
  (lambda (ma)
    (do m
        (
         (a <- ma)
         (mret m (f a))
         ))))

(defun test11 ()
  (do statemonad
      (
       (put 111)
       ((lift1 statemonad (lambda (x) (print "state is " x))) (get))
       )))
