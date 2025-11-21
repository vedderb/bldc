
(def l (list 1 2 3 4))

;; The usage of l in two places here is not
;; treated as sharing. The are simply occurrences
;; the symbol l to be looked up in the environemnt at runtime,
;; not a direct reference to the address of the list.
(defun f () (+ (apply + l) (apply + l)))



(defun main ()
  (if (eq (f) 20)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
