
(define a (cons 1 (cons 2 (cons 3 4))))

(define b '(1 . 2))

(define c '(a b c . d))

(defun main () {
       (if (and (eq a '(1 2 3 . 4))
                (eq b '(1 . 2))
                (eq c '(a b c .d )))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
