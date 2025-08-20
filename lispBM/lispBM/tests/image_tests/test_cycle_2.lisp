(define a (list 1 2 3 4))
(setcdr (cdr (cdr (cdr a))) a)

(defun main () {
       (if (and (eq (ix a 0) 1)
                (eq (ix a 1) 2)
                (eq (ix a 2) 3)
                (eq (ix a 3) 4)
                (eq (ix a 4) 1)
                (eq (ix a 5) 2)
                )
            (print "SUCCESS")
            (print "FAILURE"))
         })
       
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
