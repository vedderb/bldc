(define char-a \#a)
(define char-newline \#\n)
(define char-list '(\#h \#e \#l \#l \#o))

(defun main ()
  (if (and (eq char-a \#a)
           (eq char-newline \#\n)
           (eq (length char-list) 5))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))