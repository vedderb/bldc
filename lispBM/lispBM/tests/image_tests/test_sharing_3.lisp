@const-start
(define l (list 5 6 7))
@const-end

(define a (list 1 2 3))

(define b (cons 100 a))

(define c (cons 200 l))

(defun main () {

       (setix b 1 100)
       (setix b 2 200)
       (setix b 3 300)

       (var res (trap (setix c 2 100)))
       
       (if (and (eq a (list 100 200 300))
                (eq b (list 100 100 200 300))
                (eq c (list 200 5 6 7))
                (eq res '(exit-error type_error)))
           (print "SUCCESS")
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
