


;; Does not lead to shared nodes.
(define m (macro (c0)
                 `(+ ,c0 ,c0)))

(defun main () 
  (if (= (m (+ 1 2)) 6)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(f-write-image (f-open "image.lbm" "w"))

    
