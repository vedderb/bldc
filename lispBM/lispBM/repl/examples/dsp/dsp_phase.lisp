#!/usr/bin/env -S shlbm -M 512000 --

(import "dsp_lang.lisp")

(define buffer (bufcreate (* 4 1024)))


;; Example 6: Magnitude and Phase plot
;; Generate sin(2000Hz) + cos(2000Hz) signal

;;
;; sin(2*pi*f*t) + cos(2*pi*f*t) = sqrt(2) * sin(2*pi*f*t + (pi/4))
;;
;; So plotting magnitude after the FFT will show one peak at 2000Hz
;; The magnitude in point p is computed as sqrt((real[p] * real[p]) + (im[p] * im[p])).
;;
;; The information about the phase of this signal is also present
;; in the output from FFT and is obtained as atan2(im[p], real[p]).
;;
;; In this case the phase information should show the (pi/4) shift
;; at the position corresponding to the 2000Hz peak.
;;
;; Each bin of the FFT is a filter. This filter is not "perfect"
;; in the sense that it passes exactly the desired frequency.
;; As the filters are not perfect, the 2KHz signal will show up
;; in multiple bins (neighbouring bins). This is called spectral leakage.
;;
;; This imperfection will also mean that when we try to read out the
;; (pi / 4) phase shift in the result it will not be exacly (pi / 4).
;;

(define phase-test-sig (signal-sum (signal-sin 2000.0) (signal-cos 2000.0)))
(sample-signal phase-test-sig 20000 buffer)

(define zero-im2 (bufcreate (* 1024 4)))
(define fft-res2 (fft buffer zero-im2 'little-endian))
(define fft-r2 (car fft-res2))
(define fft-im2 (cdr fft-res2))

(define mag-buf (bufcreate (* 512 4)))
(define phase-buf (bufcreate (* 512 4)))

;; Compute magnitude and phase for first 512 bins (up to Nyquist)
;; Convert radians to degrees: degrees = radians * 180 / pi
(define rad-to-deg (/ 180.0 3.14159))
(loopfor i 0 (< i 512) (+ i 1) {
      (var real (bufget-f32 fft-r2 (* i 4) 'little-endian))
      (var imag (bufget-f32 fft-im2 (* i 4) 'little-endian))
      (var mag (sqrt (+ (* real real) (* imag imag))))
      (var phase-rad (atan2 imag real))
      (var phase-deg (* phase-rad rad-to-deg))
      (bufset-f32 mag-buf (* i 4) mag 'little-endian)
      (bufset-f32 phase-buf (* i 4) phase-deg 'little-endian)
      })


;; Averaging around the peak-bin  and calculating phase shift
;; Note that one does not need to divide the sums by 5 to "properly average them"
;; as it is irrelevant to the angle between the real axis and the real, imag vector.
(let ((peak-bin 102)
      (sum-real 0)
      (sum-imag 0)) {
      (loopfor j (- peak-bin 2) (<= j (+ peak-bin 2)) (+ j 1) {
            (var real (bufget-f32 fft-r2 (* j 4) 'little-endian))
            (var imag (bufget-f32 fft-im2 (* j 4) 'little-endian))
            (setq sum-real (+ sum-real real))
            (setq sum-imag (+ sum-imag imag))
            })
      (print "phase shift: " (atan2 sum-imag sum-real))
      }
      )

;; The phase shift computed above is not that interesting in itself.
;; A single phase shift computed for a single frequency component like this
;; depends just as much on when the sampling window started in time relative to the signal
;; as it does on the signal itself.
;;
;; Phase shifts becomes interesting when measured relative the shift of the frequency components
;; of the signal (here the phase says something about what the the wave-form looks like).
;;
;; Relative differences in phase is also interesting when comparing several simultaneous readings
;; of a signal using multiple antennas.


(define fmag (fopen "magnitude.bin" "wb"))
(fwrite fmag mag-buf)
(fclose fmag)

(define fphase (fopen "phase.bin" "wb"))
(fwrite fphase phase-buf)
(fclose fphase)

(print "output: magnitude_phase_spectrum.pdf")
(plot-magnitude-phase "magnitude.bin" "phase.bin"
                      "magnitude_phase_spectrum.pdf"
                      "FFT Analysis: sin(2000Hz) + cos(2000Hz)")
