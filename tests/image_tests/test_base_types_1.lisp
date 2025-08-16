(define a (list 1 2 3))

(define b (list 1b 2b 3b))

(define c (list 1u32 2i32 3.14))

(define d (list 1u64 2i64 3.14f64))

(define e (list 1u 2u))

(define f "hello")

(defun main () {
       (if (and (eq a (list 1 2 3))
                (eq b (list 1b 2b 3b))
                (eq c (list 1u32 2i32 3.14))
                (eq d (list 1u64 2i64 3.14f64))
                (eq e (list 1u 2u))
                (eq f "hello"))
           
           (print "SUCCESS")
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
