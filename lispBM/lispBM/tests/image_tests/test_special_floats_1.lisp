
(define nan-val (sqrt -1))
(define inf-val (exp 1000))
(define neg-zero -0.0)
(define pos-zero 0.0)
(define regular-float 3.14)

(defun main () {
       (if (and (is-nan nan-val)
                (is-inf inf-val)
                (= neg-zero pos-zero)
                (= regular-float 3.14))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
