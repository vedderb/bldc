
;; Test tree without sharing
(def tree '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))

(defun main () {
       (print tree)
       (if (eq tree '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
           (print "SUCCESS")
           (print "FAILURE")
           )
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
