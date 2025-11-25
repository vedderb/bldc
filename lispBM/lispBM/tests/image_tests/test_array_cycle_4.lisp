(define arr [| [| a b c |] 1 2 3 4|])

(setix arr 0 (setix (ix arr 0) 0 arr))

(defun main () {
       (if (and (= (ix arr 1) 1)
                (= (ix arr 2) 2)
            )
            (print "SUCCESS")
            (print "FAILURE"))
         })
       
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
