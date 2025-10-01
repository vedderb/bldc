
(def a 0)
(def s 0)
(def e 100)
(def r 0)

(def waste (range 1000))

@const-start

(define apa "-----------------------------------------------------------")

(defun g () {
       (loopwhile r {
             (if (= r 100)
                 (setq r nil)
                 (setq r (+ r 1)))
             })
       r
       })

(defun f (x y z) {
       (var e (+ x y z))
       (looprange i 0 e 
             (setq a i)
             )
       a
       })

(defun h (i j k) {
       (var e (+ i j k))
       (looprange i 100 (+ e 100)
             (setq a i)
             )
       (print "h " r)
       a
       })



(defun main () {
       (print (g))
       (print apa)
       (if (and (= (f 1 1 98) 99)
                (= (h 1 1 98) 199))
           (print "SUCCESS")
           (print "FAILURE"))
       })

@const-end

(print "Saving image")
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
