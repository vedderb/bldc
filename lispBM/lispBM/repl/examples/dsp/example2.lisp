#!/usr/bin/env -S shlbm -M 512000 --
(import "dsp_lang.lisp")

;; FFT and inverse

(define data-r (bufcreate (* 4 1024)))
(define data-im (bufcreate (* 4 1024)))
(define magnitudes (bufcreate (* 4 512)))
(bufclear data-im)

(define sample-rate 20000.0)

(define test-sig (signal-sum (signal-sum (signal-sin 1000)
                                         (signal-sin 2000))
                             (signal-sum (signal-sin 8000)
                                         (signal-sin 7000))))
(with-file "wave.bin" "wb"
           (lambda (x) (fwrite x (sample-signal test-sig 20000 data-r))))

(plot-signal "wave.bin" "example2_1.pdf" "sum of 4 sines (20kHz sample rate)")



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

(plot-signal-spectrum "wave.bin" "fft.bin" "example2_2.pdf" "Signal Analysis" "Input Wave" "Spectrum")

(with-file "recreated.bin" "wb"
           (lambda (x) (fwrite x (car recreated))))

(define gp (gnuplot-open))

(gnuplot-cmd gp "set terminal pdf size 10,8")
(gnuplot-cmd gp (str-join `("set output 'example2_3.pdf'")))

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
