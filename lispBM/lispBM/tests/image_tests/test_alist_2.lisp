
@const-start
(define alist1 (acons 'key1 'value1 nil))
(define alist2 (acons 'key2 'value2 alist1))
(define alist3 (acons 'key3 'value3 alist2))

(define manual-alist '((a . 1) (b . 2) (c . 3)))
@const-end

(defun main () {
       (if (and (eq (assoc alist3 'key1) 'value1)
                (eq (assoc alist3 'key2) 'value2)
                (eq (assoc alist3 'key3) 'value3)
                (eq (assoc manual-alist 'a) 1)
                (eq (assoc manual-alist 'b) 2)
                (eq (assoc manual-alist 'c) 3)
                (eq (cossa manual-alist 2) 'b))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
