
(define entry-correlate
  (ref-entry "correlate"
             (list
              (para (list "`correlate` computes the cross-correlation of two real-valued signals."
                          "The form is `(correlate s1 s2)` or `(correlate s1 s2 'little-endian)`."
                          "Both `s1` and `s2` must be byte arrays containing 32-bit floats."
                          "Returns a new byte array of floats with length `(+ (len s1) (len s2) -1)`,"
                          "where lengths are measured in number of floats."
                          "Cross-correlation measures the similarity between two signals"
                          "as a function of the time-lag applied to one of them."
                          ))
              (para (list "The optional `'little-endian` argument specifies the byte order of the"
                          "float data in the input arrays. If omitted the data is assumed to be"
                          "big-endian. On most embedded systems floats are stored in little-endian"
                          "format, so `'little-endian` should typically be passed."
                          ))
              (program '(((define s1 (bufcreate 16))
                          (bufset-f32 s1 0 1.0 'little-endian)
                          (bufset-f32 s1 4 2.0 'little-endian)
                          (bufset-f32 s1 8 3.0 'little-endian)
                          (bufset-f32 s1 12 4.0 'little-endian)
                          (define s2 (bufcreate 8))
                          (bufset-f32 s2 0 1.0 'little-endian)
                          (bufset-f32 s2 4 1.0 'little-endian)
                          (correlate s1 s2 'little-endian)
                          )
                         ((define s1 (bufcreate 16))
                          (bufset-f32 s1 0 1.0)
                          (bufset-f32 s1 4 2.0)
                          (bufset-f32 s1 8 3.0)
                          (bufset-f32 s1 12 4.0)
                          (define s2 (bufcreate 8))
                          (bufset-f32 s2 0 1.0)
                          (bufset-f32 s2 4 1.0)
                          (correlate s1 s2)
                          )))
              end)))

(define entry-complex-correlate
  (ref-entry "complex-correlate"
             (list
              (para (list "`complex-correlate` computes the cross-correlation of two complex-valued signals."
                          "The form is `(complex-correlate s1-re s1-im s2-re s2-im)` or with an"
                          "optional `'little-endian` as a fifth argument."
                          "All four arguments must be byte arrays of equal length containing 32-bit floats,"
                          "representing the real and imaginary parts of the two signals."
                          "Returns a cons pair `(output-re . output-im)` of byte arrays."
                          "The correlation uses the conjugate of `s2`, consistent with the standard"
                          "definition of complex cross-correlation."
                          ))
              (para (list "The real and imaginary arrays of each signal must have the same length."
                          "The optional `'little-endian` argument specifies the byte order of the float data."
                          ))
              end)))

(define entry-convolve
  (ref-entry "convolve"
             (list
              (para (list "`convolve` computes the convolution of a signal with a filter kernel."
                          "The form is `(convolve signal filter)` or `(convolve signal filter 'little-endian)`."
                          "Both arguments must be byte arrays containing 32-bit floats."
                          "Returns a new byte array of floats with length `(+ (len signal) (len filter) -1)`,"
                          "where lengths are measured in number of floats."
                          "Convolution is commonly used for applying FIR filters to signals."
                          ))
              (para (list "The optional `'little-endian` argument specifies the byte order of the"
                          "float data in the input arrays. If omitted the data is assumed to be big-endian."
                          ))
              (program '(((define signal (bufcreate 16))
                          (bufset-f32 signal 0 1.0 'little-endian)
                          (bufset-f32 signal 4 2.0 'little-endian)
                          (bufset-f32 signal 8 3.0 'little-endian)
                          (bufset-f32 signal 12 4.0 'little-endian)
                          (define kernel (bufcreate 8))
                          (bufset-f32 kernel 0 0.5 'little-endian)
                          (bufset-f32 kernel 4 0.5 'little-endian)
                          (convolve signal kernel 'little-endian)
                          )
                         ((define signal (bufcreate 16))
                          (bufset-f32 signal 0 1.0)
                          (bufset-f32 signal 4 2.0)
                          (bufset-f32 signal 8 3.0)
                          (bufset-f32 signal 12 4.0)
                          (define kernel (bufcreate 8))
                          (bufset-f32 kernel 0 0.5)
                          (bufset-f32 kernel 4 0.5)
                          (convolve signal kernel)
                          )))
              end)))

(define entry-complex-convolve
  (ref-entry "complex-convolve"
             (list
              (para (list "`complex-convolve` computes the convolution of two complex-valued signals."
                          "The form is `(complex-convolve sig-re sig-im fil-re fil-im)` or with an"
                          "optional `'little-endian` as a fifth argument."
                          "All four arguments must be byte arrays of equal length containing 32-bit floats,"
                          "representing the real and imaginary parts of the signal and filter."
                          "Returns a cons pair `(output-re . output-im)` of byte arrays."
                          ))
              (para (list "The real and imaginary arrays of each input must have the same length."
                          "The optional `'little-endian` argument specifies the byte order of the float data."
                          ))
              end)))

(define entry-fft
  (ref-entry "fft"
             (list
              (para (list "`fft` computes the Fast Fourier Transform of a signal."
                          "The form is `(fft real-arr imag-arr)` with optional additional arguments."
                          "Both `real-arr` and `imag-arr` must be byte arrays of equal size"
                          "containing 32-bit floats, representing the real and imaginary parts"
                          "of the input signal."
                          "Returns a cons pair `(real-output . imag-output)` of byte arrays."
                          ))
              (para (list "If the input length is not a power of two, the arrays are zero-padded"
                          "to the next power of two before the transform is applied."
                          "The output length will therefore be at least as large as the input."
                          ))
              (para (list "Optional arguments:"
                          ))
              (bullet (list "Pass `'inverse` to compute the inverse FFT. The result is scaled by 1/N."
                            "Pass `'little-endian` to specify that float data is in little-endian byte order."
                            "Both can be combined: `(fft re im 'inverse 'little-endian)`."
                            ))
              (para (list "For a real-valued signal, set all values in `imag-arr` to zero."
                          ))
              (program '(((define n 8)
                          (define re (bufcreate (* n 4)))
                          (define im (bufcreate (* n 4)))
                          (bufset-f32 re 0 1.0 'little-endian)
                          (bufset-f32 re 4 1.0 'little-endian)
                          (bufset-f32 re 8 1.0 'little-endian)
                          (bufset-f32 re 12 1.0 'little-endian)
                          (define result (fft re im 'little-endian))
                          (car result)
                          )
                         ((define n 8)
                          (define re (bufcreate (* n 4)))
                          (define im (bufcreate (* n 4)))
                          (bufset-f32 re 0 1.0)
                          (bufset-f32 re 4 1.0)
                          (bufset-f32 re 8 1.0)
                          (bufset-f32 re 12 1.0)
                          (define result (fft re im))
                          (car result)
                          )))
              end)))

(define chapter-real
  (section 2 "Real-valued Operations"
           (list entry-correlate
                 entry-convolve
                 )))

(define chapter-complex
  (section 2 "Complex-valued Operations"
           (list entry-complex-correlate
                 entry-complex-convolve
                 )))

(define chapter-fft
  (section 2 "Fourier Transform"
           (list entry-fft
                 )))

(define manual
  (list
   (section 1 "LispBM DSP Extensions Reference Manual"
            (list
             (para (list "The DSP extensions provide digital signal processing functions"
                         "such as correlation, convolution and fast Fourier transform."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             (para (list "All DSP functions operate on byte arrays containing 32-bit floats."
                         "The byte order of float data in the arrays can be specified with"
                         "the `'little-endian` symbol. If not specified, big-endian is assumed."
                         "On most embedded platforms (ARM, x86) floats are stored in"
                         "little-endian byte order, so `'little-endian` should typically be passed."
                         ))
             chapter-real
             chapter-complex
             chapter-fft
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "dspref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "DSP extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
