
(defun mk-deep-list (n acc)
  (if (= n 0) acc
      (mk-deep-list (- n 1) (list n acc))))


@const-start
; Create a deeply nested structure with 105 levels
(define deep-list (mk-deep-list 55 '(56)))
@const-end

(defun verify-depth (lst depth)
  (if (= depth 57) t
      (if (= (car lst) depth)
          (verify-depth (car (cdr lst)) (+ depth 1))
          nil)))

(defun main () {
       (if (verify-depth deep-list 1)
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
