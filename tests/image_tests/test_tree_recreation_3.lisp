
;; Test tree with sharing
(def t1 (list 1 2 3))
(def t11 (list 4 5 6))
(def t2 (list t1 t11))
(def t3 (list t2 t2))

(def lt (list t3 t3 t3 t3))

(defun main () {
       (if (and (eq (ix lt 0)  '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
                (eq (ix lt 1)  '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
                (eq (ix lt 2)  '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
                (eq (ix lt 3)  '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6)))))
                
           (print "SUCCESS")
           (print "FAILURE")
           )
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
