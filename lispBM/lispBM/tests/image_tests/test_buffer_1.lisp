
(define buf-lit [1 2 3 4])

(define buf (bufcreate 4))

(bufset-u8 buf 0 0)
(bufset-u8 buf 1 1)
(bufset-u8 buf 2 2)
(bufset-u8 buf 3 3)

(print buf-lit)
(print buf)

(defun main () {
       (print buf-lit)
       (print buf)
       (if (and (eq buf-lit [1 2 3 4])
                (eq buf [0 1 2 3]))
           (print "SUCCESS")
           (print "FAILURE"))

       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
