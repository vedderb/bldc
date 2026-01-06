#!/usr/bin/env -S shlbm -M 512000 --
(import "dsp_lang.lisp")

;; The effects of hamming window to the FFT result

(define data-r (bufcreate (* 4 1024)))
(define data-im (bufcreate (* 4 1024)))
(define magnitudes (bufcreate (* 4 512)))
(bufclear data-im)

(define sample-rate 20000.0)

(define test-sig (signal-const 0))

;; Create a more involved signal.
(loopfor i 0 (< i 10) (+ i 1) {
      (setq test-sig (signal-sum (signal-sin (* i 1000)) test-sig))
      })

;; Create data-r contents
(sample-signal test-sig 20000 data-r)

(with-file "wave.bin" "wb"
           (lambda (x) (fwrite x data-r)))

(plot-signal "wave.bin" "example4_1a.pdf" "sum of 4 sines (20kHz sample rate)")

(defun hamming (n m)
  (- 0.54 (* 0.46 (cos (/ (* 2.0 pi n) (- m 1.0))))))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var v (bufget-f32 data-r (* i 4) 'little-endian))
      (bufset-f32 data-r (* i 4) (* v (hamming i 1024)) 'little-endian) 
      })

(with-file "wave.bin" "wb"
           (lambda (x) (fwrite x data-r)))

(plot-signal "wave.bin" "example4_1b.pdf" "sum of 4 sines (20kHz sample rate)")


(let ((fft-r (fft data-r data-im 'little-endian)))
  {
  (loopfor i 0 (< i 512) (+ i 1) {
        (var x (bufget-f32 (car fft-r) (* i 4) 'little-endian))
        (var y (bufget-f32 (cdr fft-r) (* i 4) 'little-endian))
        (bufset-f32 magnitudes (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
        })
  (define recreated (fft (car fft-r) (cdr fft-r)  'inverse 'little-endian))
  })

(with-file "fft.bin" "wb"
           (lambda (x) (fwrite x magnitudes)))

(plot-signal-spectrum "wave.bin" "fft.bin" "example4_2.pdf" "Signal Analysis" "Input Wave" "Spectrum")

(with-file "recreated.bin" "wb"
           (lambda (x) (fwrite x (car recreated))))

(define gp (gnuplot-open))

(gnuplot-cmd gp "set terminal pdf size 10,8")
(gnuplot-cmd gp (str-join `("set output 'example4_3.pdf'")))

(gnuplot-cmd gp "set multiplot layout 3,1 title 'Lisp DSP'")

(gnuplot-cmd gp "set title 'Original Waveform'")
(gnuplot-cmd gp "set xlabel 'Sample'")
(gnuplot-cmd gp "set ylabel 'Amplitude'")
(gnuplot-cmd gp "plot 'wave.bin' binary array=1024 format='%float' with lines title 'Time Domain'")

(gnuplot-cmd gp "set title 'FFT'")
(gnuplot-cmd gp "set xlabel 'Frequency Bin'")
(gnuplot-cmd gp "set ylabel 'Magnitude'")
(gnuplot-cmd gp "plot 'fft.bin' binary array=512 format='%float' with lines title 'Frequency Domain'")

(gnuplot-cmd gp "set title 'Recreated Waveform'")
(gnuplot-cmd gp "set xlabel 'Sample'")
(gnuplot-cmd gp "set ylabel 'Amplitude'")
(gnuplot-cmd gp "plot 'recreated.bin' binary array=1024 format='%float' with lines title 'Time Domain'")

(gnuplot-cmd gp "unset multiplot")

(gnuplot-close gp)
