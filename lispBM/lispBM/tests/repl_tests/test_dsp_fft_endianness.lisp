; Test FFT with different endianness modes

(defun test-fft-little-endian ()
  ; Test FFT with explicit little-endian mode
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufclear real 0 0 16)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag 'little-endian)))
        (not (eq result nil)))
    }))

(defun test-fft-big-endian ()
  ; Test FFT with explicit big-endian mode
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufclear real 0 0 16)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag 'big-endian)))
        (not (eq result nil)))
    }))

(defun test-fft-inverse-with-endian ()
  ; Test FFT with both inverse and endianness flags
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufset-f32 real 0 1.0f32)
      (bufset-f32 real 4 2.0f32)
      (bufset-f32 real 8 3.0f32)
      (bufset-f32 real 12 4.0f32)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag 'inverse 'little-endian)))
        (not (eq result nil)))
    }))

(defun test-fft-endian-order-variant1 ()
  ; Test with little-endian as 3rd argument
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufclear real 0 0 16)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag 'little-endian)))
        (not (eq result nil)))
    }))

(defun test-fft-endian-order-variant2 ()
  ; Test with little-endian as 4th argument (after inverse)
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufclear real 0 0 16)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag 'inverse 'little-endian)))
        (not (eq result nil)))
    }))

(defun run-tests ()
  (and (test-fft-little-endian)
       (test-fft-big-endian)
       (test-fft-inverse-with-endian)
       (test-fft-endian-order-variant1)
       (test-fft-endian-order-variant2)))

(if (run-tests)
  (print "SUCCESS")
  (print "FAIL"))
