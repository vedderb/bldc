; Test FFT with various buffer sizes including padding

(defun test-fft-size-8-bytes ()
  ; 8 bytes = 2 floats (power of 2)
  (let ((real (bufcreate 8))
        (imag (bufcreate 8)))
    {
      (bufclear real 0 0 8)
      (bufclear imag 0 0 8)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 8)
               (= (buflen (cdr result)) 8))))
    }))

(defun test-fft-size-16-bytes ()
  ; 16 bytes = 4 floats (power of 2)
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufclear real 0 0 16)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 16)
               (= (buflen (cdr result)) 16))))
    }))

(defun test-fft-size-32-bytes ()
  ; 32 bytes = 8 floats (power of 2)
  (let ((real (bufcreate 32))
        (imag (bufcreate 32)))
    {
      (bufclear real 0 0 32)
      (bufclear imag 0 0 32)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 32)
               (= (buflen (cdr result)) 32))))
    }))

(defun test-fft-size-64-bytes ()
  ; 64 bytes = 16 floats (power of 2)
  (let ((real (bufcreate 64))
        (imag (bufcreate 64)))
    {
      (bufclear real 0 0 64)
      (bufclear imag 0 0 64)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 64)
               (= (buflen (cdr result)) 64))))
    }))

(defun test-fft-padding-12-to-16 ()
  ; 12 bytes = 3 floats, should pad to 16 bytes (4 floats)
  (let ((real (bufcreate 12))
        (imag (bufcreate 12)))
    {
      (bufclear real 0 0 12)
      (bufclear imag 0 0 12)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 16)
               (= (buflen (cdr result)) 16))))
    }))

(defun test-fft-padding-20-to-32 ()
  ; 20 bytes = 5 floats, should pad to 32 bytes (8 floats)
  (let ((real (bufcreate 20))
        (imag (bufcreate 20)))
    {
      (bufclear real 0 0 20)
      (bufclear imag 0 0 20)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 32)
               (= (buflen (cdr result)) 32))))
    }))

(defun test-fft-padding-24-to-32 ()
  ; 24 bytes = 6 floats, should pad to 32 bytes (8 floats)
  (let ((real (bufcreate 24))
        (imag (bufcreate 24)))
    {
      (bufclear real 0 0 24)
      (bufclear imag 0 0 24)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 32)
               (= (buflen (cdr result)) 32))))
    }))

(defun test-fft-padding-28-to-32 ()
  ; 28 bytes = 7 floats, should pad to 32 bytes (8 floats)
  (let ((real (bufcreate 28))
        (imag (bufcreate 28)))
    {
      (bufclear real 0 0 28)
      (bufclear imag 0 0 28)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 32)
               (= (buflen (cdr result)) 32))))
    }))

(defun test-fft-padding-36-to-64 ()
  ; 36 bytes = 9 floats, should pad to 64 bytes (16 floats)
  (let ((real (bufcreate 36))
        (imag (bufcreate 36)))
    {
      (bufclear real 0 0 36)
      (bufclear imag 0 0 36)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (= (buflen (car result)) 64)
               (= (buflen (cdr result)) 64))))
    }))

(defun run-tests ()
  (and (test-fft-size-8-bytes)
       (test-fft-size-16-bytes)
       (test-fft-size-32-bytes)
       (test-fft-size-64-bytes)
       (test-fft-padding-12-to-16)
       (test-fft-padding-20-to-32)
       (test-fft-padding-24-to-32)
       (test-fft-padding-28-to-32)
       (test-fft-padding-36-to-64)))

(if (run-tests)
  (print "SUCCESS")
  (print "FAIL"))
