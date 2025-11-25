
(define buf-lit [1 2 3 4 5 6 7])

(define buf (bufcreate 7))

(bufset-u8 buf 0 0)
(bufset-u8 buf 1 1)
(bufset-u8 buf 2 2)
(bufset-u8 buf 3 3)
(bufset-u8 buf 4 4)
(bufset-u8 buf 5 5)
(bufset-u8 buf 6 6)


(print buf-lit)
(print buf)

(defun main () {
       (print buf-lit)
       (print buf)
       (if (and (eq buf-lit [1 2 3 4 5 6 7])
                (eq buf [0 1 2 3 4 5 6]))
           (print "SUCCESS")
           (print "FAILURE"))

       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
