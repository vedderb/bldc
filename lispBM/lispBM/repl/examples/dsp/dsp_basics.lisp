#!/usr/bin/env -S shlbm -M 512000 --

(import "dsp_lang.lisp")

(define buffer (bufcreate (* 4 1024)))

;; Example 1 plot a 440Hz sine
(define sine-sig (signal-sin 440.0))

;; Example of bracket operation
(with-file "wave.bin" "wb"
           (lambda (x) (fwrite x (sample-signal sine-sig 20000 buffer))))

(print "output: sin440.pdf")
(plot-signal "wave.bin" "sin440.pdf" "440Hz sine (20kHz sample rate)")

;; Example 2 plot a sum signal
(define sum-sig (signal-sum (signal-sin 440.0) (signal-sin 2400.0)))

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 (sample-signal sum-sig 20000 buffer))
(fclose f1)

(print "output: sin440_plus_2400.pdf")
(plot-signal "wave.bin" "sin440_plus_2400.pdf" "440Hz + 2400Hz")


;; Example 3 fft

(define zero-im (bufcreate (* 1024 4)))
;; buffer already contains the 440 + 2400 signal
(define fft-res (fft buffer zero-im 'little-endian))
(define fft-r (car fft-res))
(define fft-im (cdr fft-res))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var x (bufget-f32 fft-r (* i 4) 'little-endian))
      (var y (bufget-f32 fft-im (* i 4) 'little-endian))
      (bufset-f32 fft-r (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
      })


(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 fft-r)
(fclose f1)

(print "output: fft_sin440_plus_2400.pdf")
(plot-spectrum "wave.bin" "fft_sin440_plus_2400.pdf" "Frequency Spectrum: 440Hz + 2400Hz")

;; Example 3b NOISY

(define noisy-sig (signal-sum (signal-noise 1.0) (signal-sin 2400.0 0.0 1.0)))

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 (sample-signal noisy-sig 20000 buffer))
(fclose f1)

(print "output: noise_plus_2400.pdf")
(plot-signal "wave.bin" "noise_plus_2400.pdf" "noise + 2400Hz")

(define zero-im (bufcreate (* 1024 4)))
;; buffer already contains the 440 + 2400 signal
(define fft-res (fft buffer zero-im 'little-endian))
(define fft-r (car fft-res))
(define fft-im (cdr fft-res))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var x (bufget-f32 fft-r (* i 4) 'little-endian))
      (var y (bufget-f32 fft-im (* i 4) 'little-endian))
      (bufset-f32 fft-r (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
      })


(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 fft-r)
(fclose f1)

(print "output: fft_noise_plus_2400.pdf")
(plot-spectrum "wave.bin" "fft_noise_plus_2400.pdf" "Frequency Spectrum: noise + 2400Hz")



;; Example 4 plot a sum signal
(define sum-sig (signal-sum (signal-sin 2000.0) (signal-cos 2000.0)))

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 (sample-signal sum-sig 20000 buffer))
(fclose f1)

(print "output: sin2000_plus_cos2000.pdf")
(plot-signal "wave.bin" "sin2000_plus_cos2000.pdf" "sin + cos")

;; Example 5 fft

(define zero-im (bufcreate (* 1024 4)))
;; buffer already contains the sin2000 + cos2000
(define fft-res (fft buffer zero-im 'little-endian))
(define fft-r (car fft-res))
(define fft-im (cdr fft-res))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var x (bufget-f32 fft-r (* i 4) 'little-endian))
      (var y (bufget-f32 fft-im (* i 4) 'little-endian))
      (bufset-f32 fft-r (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
      })


(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 fft-r)
(fclose f1)

(print "output: fft_sin2000_plus_cos2000.pdf")
(plot-spectrum "wave.bin" "fft_sin2000_plus_cos2000.pdf" "Frequency Spectrum: 2000Hz")

