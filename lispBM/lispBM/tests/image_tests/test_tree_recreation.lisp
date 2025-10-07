
;; Test tree with sharing
(def t1 (list 1 2 3))
(def t11 (list 4 5 6))
(def t2 (list t1 t11))
(def t3 (list t2 t2))

(defun main () {
       (if (eq t3 '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
           (print "SUCCESS")
           (print "FAILURE")
           )
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
