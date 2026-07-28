

;; On the incremental reader (reg-dyn-ext) can run before the reader processes
;; ext-d1.

(reg-dyn-ext2)

(defun main () {
       (if (and (eq (ext-d2 42) nil)
                (eq (ext-d2 10) 42)
                (eq (ext-d2 1000) 10))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(f-write-image (f-open "image.lbm" "w"))
