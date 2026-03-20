
(define neg-int -42)
(define neg-int32 -2147483648i32)
(define neg-int64 -9223372036854775808i64)
(define neg-float -3.14)
(define neg-double -2.718281828459045)

(define neg-list '(-1 -2 -3 -4 -5))
(define mixed-list '(-10 20 -30 40 -50))
(define nested-neg-list '((-1 -2) (-3 -4) (-5 -6)))
(define neg-float-list '(-1.1 -2.2 -3.3 -4.4))

(defun main () {
       (if (and (= neg-int -42)
                (= neg-int32 -2147483648i32)
                (= neg-int64 -9223372036854775808i64)
                (= neg-float -3.14)
                (= neg-double -2.718281828459045)
                (eq neg-list '(-1 -2 -3 -4 -5))
                (eq mixed-list '(-10 20 -30 40 -50))
                (eq nested-neg-list '((-1 -2) (-3 -4) (-5 -6)))
                (eq neg-float-list '(-1.1 -2.2 -3.3 -4.4)))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
