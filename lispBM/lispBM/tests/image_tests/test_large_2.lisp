(define a (list 'apa 'bepa 'cepa 'depa))
(define b (list "Hello1"
                "Hello2"
                "Hello3"
                "Hello4"
                "Hello5"
                "Hello6"
                "Hello7"
                "Hello8"
                "Hello9"
                ))

(defun sixth-char (x)
  (bufget-u8 x 5))

(defun ok ()
  (and (eq (sixth-char (ix b 0)) (+ \#0 1))
       (eq (sixth-char (ix b 1)) (+ \#0 2))
       (eq (sixth-char (ix b 2)) (+ \#0 3))
       (eq (sixth-char (ix b 3)) (+ \#0 4))
       (eq (sixth-char (ix b 4)) (+ \#0 5))
       (eq (sixth-char (ix b 5)) (+ \#0 6))
       (eq (sixth-char (ix b 6)) (+ \#0 7))
       (eq (sixth-char (ix b 7)) (+ \#0 8))
       (eq (sixth-char (ix b 8)) (+ \#0 9))))
       
(define success-string "SUCCESS")

(defun main () {
       (if (ok)
           (print success-string)
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
