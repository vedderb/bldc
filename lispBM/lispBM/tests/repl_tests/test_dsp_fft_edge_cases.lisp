; Test FFT edge cases and error handling

(defun test-fft-too-few-args ()
  ; FFT requires at least 2 arguments
  (let ((real (bufcreate 16)))
    (eq (trap (fft real)) '(exit-error type_error))))

(defun test-fft-wrong-type-first-arg ()
  ; First argument must be an array
  (let ((imag (bufcreate 16)))
    (eq (trap (fft 42 imag)) '(exit-error type_error))))

(defun test-fft-wrong-type-second-arg ()
  ; Second argument must be an array
  (let ((real (bufcreate 16)))
    (eq (trap (fft real 42)) '(exit-error type_error))))

(defun test-fft-both-args-wrong-type ()
  ; Both arguments must be arrays
  (eq (trap (fft 42 "hello")) '(exit-error type_error)))

(defun test-fft-size-mismatch ()
  ; Real and imag arrays must be same size
  (let ((real (bufcreate 16))
        (imag (bufcreate 8)))
    (bufclear real 0 0 16)
    (bufclear imag 0 0 8)
    (eq (fft real imag) nil)))

(defun test-fft-too-small ()
  ; Arrays must be larger than 4 bytes (size of one float)
  (let ((real (bufcreate 4))
        (imag (bufcreate 4)))
    (bufclear real 0 0 4)
    (bufclear imag 0 0 4)
    (eq (fft real imag) nil)))

(defun test-fft-not-multiple-of-float ()
  ; Array size must be multiple of 4 (sizeof float32)
  (let ((real (bufcreate 10))
        (imag (bufcreate 10)))
    (bufclear real 0 0 10)
    (bufclear imag 0 0 10)
    (eq (fft real imag) nil)))

(defun test-fft-minimum-valid-size ()
  ; Minimum valid size is 8 bytes (2 floats)
  (let ((real (bufcreate 8))
        (imag (bufcreate 8)))
    (bufset-f32 real 0 1.0f32)
    (bufset-f32 real 4 2.0f32)
    (bufclear imag 0 0 8)
    (not (eq (fft real imag) nil))))

(defun test-fft-invalid-third-arg ()
  ; Third argument should be a symbol if provided
  ; Non-symbol values are ignored
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    (bufclear real 0 0 16)
    (bufclear imag 0 0 16)
    (not (eq (fft real imag 42) nil))))

(defun test-fft-invalid-fourth-arg ()
  ; Fourth argument should be a symbol if provided
  ; Non-symbol values are ignored
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    (bufclear real 0 0 16)
    (bufclear imag 0 0 16)
    (not (eq (fft real imag 'inverse 42) nil))))

(defun test-fft-unknown-symbol-args ()
  ; Unknown symbols are ignored, FFT still works
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    (bufclear real 0 0 16)
    (bufclear imag 0 0 16)
    (not (eq (fft real imag 'unknown-symbol) nil))))

(defun run-tests ()
  (and (test-fft-too-few-args)
       (test-fft-wrong-type-first-arg)
       (test-fft-wrong-type-second-arg)
       (test-fft-both-args-wrong-type)
       (test-fft-size-mismatch)
       (test-fft-too-small)
       (test-fft-not-multiple-of-float)
       (test-fft-minimum-valid-size)
       (test-fft-invalid-third-arg)
       (test-fft-invalid-fourth-arg)
       (test-fft-unknown-symbol-args)))

(if (run-tests)
  (print "SUCCESS")
  (print "FAIL"))
