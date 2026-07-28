(define custom-symbol 'my-special-symbol)
(define symbol-list '(a b c d))

(defun main ()
  (if (and (eq custom-symbol 'my-special-symbol)
           (eq symbol-list '(a b c d)))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(f-write-image (f-open "image.lbm" "w"))