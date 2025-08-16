(define a (list 1 2 3))

(define b (cons 100 a))
(define c (cons 200 b))
(define d (cons 300 c))
(define e (cons 400 d))

(defun main () {

       (setix e 5 1000)
       
       
       (if (and (eq a (list 1 1000 3))
                (eq e (list 400 300 200 100 1 1000 3)))
           (print "SUCCESS")
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
