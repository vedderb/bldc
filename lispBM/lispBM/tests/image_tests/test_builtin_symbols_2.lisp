
@const-start
(define plus-sym '+)
(define minus-sym '-)
(define mul-sym '*)
(define div-sym '/)
(define car-sym 'car)
(define cdr-sym 'cdr)
(define cons-sym 'cons)
(define eq-sym 'eq)
(define list-sym 'list)
(define lambda-sym 'lambda)

(define builtin-list '(+ - * / car cdr cons eq list lambda))
@const-end

(defun main () {
       (if (and (eq plus-sym '+)
                (eq minus-sym '-)
                (eq mul-sym '*)
                (eq div-sym '/)
                (eq car-sym 'car)
                (eq cdr-sym 'cdr)
                (eq cons-sym 'cons)
                (eq eq-sym 'eq)
                (eq list-sym 'list)
                (eq lambda-sym 'lambda)
                (eq builtin-list '(+ - * / car cdr cons eq list lambda)))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
