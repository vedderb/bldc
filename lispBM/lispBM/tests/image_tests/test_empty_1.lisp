
(defun main () {
  ;; Test that "empty" image boots and RTS is operational
  (var a (+ 1 2))
  (if (= a 3)
      (print "SUCCESS")
    (print "FAILURE"))
  })

(image-save)
(f-write-image (f-open "image.lbm" "w"))
