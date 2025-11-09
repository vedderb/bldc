
;; Create a deeply nested structure with 105 levels

(defun mk-deep-list (n acc)
  (if (= n 0) acc
      (mk-deep-list (- n 1) (list n acc))))
  
(define deep-list (mk-deep-list 104 '(105)))

(defun verify-depth (lst depth)
  (if (= depth 106) t
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
