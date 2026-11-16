
;; This example show how to use tinyml to open and run an ML model
;; compiled with onnx2c and linked into the firmware. 

(import "examples/tinyml/tensorlib.lisp" 'tensorlib-src)
(read-eval-program tensorlib-src)

(define h (tinyml-open "moons"))
(define in-type (tinyml-input-type h 0))
(define out-type (tinyml-output-type h 0))
(define in-bytes (tinyml-input-bytes h 0))
(define out-bytes (tinyml-output-bytes h 0))

(defun classify (x y)
  (let ((in (bufcreate in-bytes))
        (out (bufcreate out-bytes)))
    (progn
      (tensor-set in-type '(0) in x)
      (tensor-set in-type '(1) in y)
      (tinyml-run h in out)
      (let ((scores (tensor-to-list out-type '() out)))
        (if (> (car (cdr scores)) (car scores)) 1 0)))))

(define cases (list
               (list -1.0 0.4 0)
               (list 0.0 1.0 0)
               (list 1.0 -0.4 1)
               (list 2.0 0.4 0)
               (list 0.5 -0.3 1)))

(defun check-cases (cs)
  (if (eq cs nil)
      t
      (let ((c (car cs)))
        (let ((x (car c))
              (y (car (cdr c)))
              (expected (car (cdr (cdr c)))))
          (let ((got (classify x y)))
            (progn
              (print (list x y "-> class" got "expected" expected))
              (if (= got expected)
                  (check-cases (cdr cs))
                  nil)))))))

(if (check-cases cases)
    (print "SUCCESS")
    (print "FAILURE"))
