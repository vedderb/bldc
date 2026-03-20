
@const-start
(define x 42)
(define y 'foo)
(define lst '(1 2 3))

; Basic quasiquotation with unquote
(define qq1 `(a b ,x d))

; Quasiquotation with unquote-splicing
(define qq2 `(before ,@lst after))

; Nested quasiquotation
(define qq3 `(x is ,x and y is ,y))

; Mixed quasiquotation
(define qq4 `(plain ,x spliced ,@lst symbol ,y))
@const-end

(defun main () {
       (if (and (eq qq1 '(a b 42 d))
                (eq qq2 '(before 1 2 3 after))
                (eq qq3 '(x is 42 and y is foo))
                (eq qq4 '(plain 42 spliced 1 2 3 symbol foo)))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
