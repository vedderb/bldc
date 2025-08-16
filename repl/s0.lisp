
@const-start

(define ca (list 6 6 6))
(define cb (list 9 9 9))

@const-end

(define sc (list ca cb))

(define a (list 1 2 3))
(define b (cons 1 a))

(define c (list 4 5 6))
(define d (list 7 8 9))

(define e (list c d))

(define barr0 [1 2 3 4])
(define barr-pair (cons barr0 [0 0 0 0]))

(define arr0 [| 1 2 3 |])
(define arr-pair (cons arr0 [| 9 8 7 |]))

(define b1 (cons 2 a))
(define e1 (list c d))

(define ls (list 1 2 3))
(setcdr (cdr (cdr ls)) ls)

(define ls1 (list 1))
(setcdr ls1 ls1)

;; boxed values are shared.
(define f0 3.14)
(define f-pair (cons f0 f0))
(define f-pair2 (cons 3.14 3.14))

(define ls2 (list 10 11 12))
(define ls3 (cons 100 ls2))
(define ls4 (cons 200 ls3))
(define ls5 (cons 300 ls4))


;; non-boxed values are not shared.
;(define i0 23)
;(define i_pair (cons i0 i0))

(defun main () {
  (print "booting image")

  (print (eq b (list 1 1 2 3)))
  (print (eq e (list (list 4 5 6) (list 7 8 9))))
  (print (eq barr-pair (cons [1 2 3 4] [0 0 0 0])))
  (print (eq arr-pair  (cons [| 1 2 3 |] [| 9 8 7 |])))
  (print (eq b1 (list 2 1 2 3 )))
  (print (eq e1 (list (list 4 5 6) (list 7 8 9))))
  (print (eq f-pair (cons 3.14 3.14)))
  (print (eq f-pair2 (cons 3.14 3.14)))
  (print (eq (ix ls 3) 1))
  (print (eq (ix ls 4) 2))
  (print (eq (ix ls 5) 3))
  (print (eq (ix ls1 100) 1))
  (print (eq ls2 (list 10 11 12)))
  (print (eq ls3 (list 100 10 11 12)))
  (print (eq ls4 (list 200 100 10 11 12)))
  (print (eq ls5 (list 300 200 100 10 11 12)))
  (setix ls5 4 1000)
  (print (eq ls2 (list 10 1000 12)))
  (print (eq ls3 (list 100 10 1000 12)))
  (print (eq ls4 (list 200 100 10 1000 12)))
  (print (eq ls5 (list 300 200 100 10 1000 12)))
  }
  )

(image-save)
(fwrite-image (fopen "s0.lbm" "w"))
