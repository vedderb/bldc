#!/usr/bin/env -S shlbm -M 512000 --

(import "dsp_lang.lisp")


(define sample-rate 20000.0)
(define sampling-time (/ 1024.0 sample-rate))

(define my-chirp (signal-sin-chirp-linear 100 1000 sampling-time))

(define sample-buffer (bufcreate (* 4 1024)))
(sample-signal my-chirp sample-rate sample-buffer)

(with-file "wave.bin" "wb"
           (lambda (x) (fwrite x sample-buffer)))

(plot-signal "wave.bin" "chirp_1.pdf" "chirp")


(define data-r sample-buffer)
(define data-im (bufcreate (* 4 1024)))
(define magnitudes (bufcreate (* 4 512)))

(let ((fft-r (fft data-r data-im 'little-endian)))
  (loopfor i 0 (< i 512) (+ i 1) {
        (var x (bufget-f32 (car fft-r) (* i 4) 'little-endian))
        (var y (bufget-f32 (cdr fft-r) (* i 4) 'little-endian))
        (bufset-f32 magnitudes (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
        })
  )
  
(with-file "fft.bin" "wb"
           (lambda (x) (fwrite x magnitudes)))

(plot-signal-spectrum "wave.bin" "fft.bin" "chirp_2.pdf" "chirp with spectrum" "chirp" "spectrum")
