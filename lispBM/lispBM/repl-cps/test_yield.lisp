
;; while (true) {
;;  usleep(500000);
;;  printf("hello %d\n", x++);
;; }

(let ((f (lambda (x)
	   (progn
	     (yield 500000)
	     (print "hello " x \#newline)
	     (f (+ x 1))))))
  (f 0))


	     
