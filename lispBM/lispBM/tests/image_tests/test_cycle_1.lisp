(define a (list 1 2 3))
(setcdr (cdr (cdr a)) a)

(defun main () {
       (if (and (eq (ix a 0) 1)
                (eq (ix a 1) 2)
                (eq (ix a 2) 3)
                (eq (ix a 3) 1)
                (eq (ix a 4) 2))
            (print "SUCCESS")
            (print "FAILURE"))
         })
       
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
