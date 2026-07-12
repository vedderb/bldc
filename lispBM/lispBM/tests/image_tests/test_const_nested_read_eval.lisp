
@const-start

(read-eval-program "(define lib-no-const (list 1 2 3))")

(read-eval-program "@const-start (define lib-const (list 4 5 6)) @const-end")

(define outer-after (list 7 8 9))

@const-end

(define apa (list 1 2 3 4 5))

(defun main () {
       (if (and (not (constant? lib-no-const))
                (constant? lib-const)
                (constant? outer-after)
                (eq lib-no-const (list 1 2 3))
                (eq lib-const (list 4 5 6))
                (eq outer-after (list 7 8 9))
                (eq apa (list 1 2 3 4 5)))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(f-write-image (f-open "image.lbm" "w"))
