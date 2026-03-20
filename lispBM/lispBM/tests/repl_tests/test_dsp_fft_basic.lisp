; Test basic FFT functionality
; Tests forward FFT with simple inputs

(defun approx-equal (x y tolerance)
  (< (abs (- x y)) tolerance))

(defun test-fft-zero-signal ()
  ; FFT of all zeros should be all zeros
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufclear real 0 0 16)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (let ((real-out (car result))
                (imag-out (cdr result)))
            (and (eq (bufget-f32 real-out 0) 0.0f32)
                 (eq (bufget-f32 real-out 4) 0.0f32)
                 (eq (bufget-f32 imag-out 0) 0.0f32)
                 (eq (bufget-f32 imag-out 4) 0.0f32)))))
    }))

(defun test-fft-dc-signal ()
  ; FFT of DC signal (all same value) should have energy only at DC (index 0)
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufset-f32 real 0 1.0f32)
      (bufset-f32 real 4 1.0f32)
      (bufset-f32 real 8 1.0f32)
      (bufset-f32 real 12 1.0f32)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (let ((real-out (car result))
                (imag-out (cdr result)))
            ; DC component should be 4.0 (sum of all inputs)
            ; All other components should be ~0
            (and (approx-equal (bufget-f32 real-out 0) 4.0f32 0.001f32)
                 (approx-equal (bufget-f32 real-out 4) 0.0f32 0.001f32)
                 (approx-equal (bufget-f32 imag-out 0) 0.0f32 0.001f32)))))
    }))

(defun test-fft-power-of-two ()
  ; Test FFT with power-of-2 size (4 floats = 16 bytes)
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufset-f32 real 0 1.0f32)
      (bufset-f32 real 4 0.0f32)
      (bufset-f32 real 8 1.0f32)
      (bufset-f32 real 12 0.0f32)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag)))
        (not (eq result nil)))
    }))

(defun test-fft-non-power-of-two ()
  ; Test FFT with non-power-of-2 size (gets padded to next power of 2)
  ; 3 floats = 12 bytes, should be padded to 4 floats
  (let ((real (bufcreate 12))
        (imag (bufcreate 12)))
    {
      (bufset-f32 real 0 1.0f32)
      (bufset-f32 real 4 2.0f32)
      (bufset-f32 real 8 3.0f32)
      (bufclear imag 0 0 12)
      (let ((result (fft real imag)))
        ; Should return padded arrays (16 bytes = 4 floats)
        (if (eq result nil)
          nil
          (let ((real-out (car result))
                (imag-out (cdr result)))
            (and (= (buflen real-out) 16)
                 (= (buflen imag-out) 16)))))
    }))

(defun test-fft-returns-cons ()
  ; Test that FFT returns a cons pair (real . imag)
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    {
      (bufclear real 0 0 16)
      (bufclear imag 0 0 16)
      (let ((result (fft real imag)))
        (if (eq result nil)
          nil
          (and (list? result))))
    }))

(define r1 (test-fft-zero-signal))
(if (not r1) (print "Test 1 failed"))

(define r2 (test-fft-dc-signal))
(if (not r2) (print "Test 2 failed"))
  
(define r3 (test-fft-power-of-two))
(if (not r3) (print "Test 3 failed"))

(define r4 (test-fft-non-power-of-two))
(if (not r4) (print "Test 4 failed"))

(define r5 (test-fft-returns-cons))
(if (not r5) (print "Test 5 failed"))



(defun check-em ()
  (and r1 r2 r3 r4 r5))

(if (check-em)
  (print "SUCCESS")
  (print "FAIL"))
