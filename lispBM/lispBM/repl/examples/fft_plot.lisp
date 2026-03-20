

(define gp (gnuplot-open))

(define data-r (bufcreate (* 4 1024)))
(define data-im (bufcreate (* 4 1024)))

(define sample-rate 20000.0)

(define t1-delta (/ (* 220 (* 2 3.14159)) sample-rate)) 
(define t1-now 0)

(define t2-delta (/ (* 5000 (* 2 3.14159)) sample-rate)) 
(define t2-now 0)

;; the combination of the 220Hz and the 5kHz signals
;; will show peaks at:
;;   220Hz -> peak at (* (/ 220.0 20000.0) 1024) ~11
;;   5kHz  -> peak at (* (/ 5000.0 20000.0) 1024) ~256
;; after fft.

(loopfor i 0 (< i 1024) (+ i 1) {
      (bufset-f32 data-r (* i 4) (+ (sin t1-now) (sin t2-now)) 'little-endian)
      (bufset-f32 data-im (* i 4) 0.0f32 'little-endian)
      (setq t1-now (+ t1-now t1-delta))
      (setq t2-now (+ t2-now t2-delta))
      })

(define res (fft data-r data-im 'little-endian))

(define r-r (car res))
(define r-im (cdr res))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var x (bufget-f32 r-r (* i 4) 'little-endian))
      (var y (bufget-f32 r-im (* i 4) 'little-endian))
      (bufset-f32 r-r (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
      })

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 data-r)
(fclose f1)

(define f2 (fopen "fft.bin" "wb"))
(fwrite f2 r-r)
(fclose f2)

(gnuplot-cmd gp "set multiplot layout 1,2 title 'Signal Analysis'")

(gnuplot-cmd gp "set title 'Original Waveform'")
(gnuplot-cmd gp "set xlabel 'Sample'")
(gnuplot-cmd gp "set ylabel 'Amplitude'")
(gnuplot-cmd gp "plot 'wave.bin' binary array=1024 format='%float' with lines title 'Time Domain'")

(gnuplot-cmd gp "set title 'FFT'")
(gnuplot-cmd gp "set xlabel 'Frequency Bin'")
(gnuplot-cmd gp "set ylabel 'Magnitude'")
(gnuplot-cmd gp "plot 'fft.bin' binary array=512 format='%float' with lines title 'Frequency Domain'")

(gnuplot-cmd gp "unset multiplot")

(gnuplot-close gp)
