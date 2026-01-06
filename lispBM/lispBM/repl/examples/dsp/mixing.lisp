#!/usr/bin/env -S shlbm -M 512000 --

(import "dsp_lang.lisp")

;; in radio mixing is multiplication.
;; This example shows DSB-SC (Double-Sideband Suppressed Carrier)  modulation

(define baseband-sig (signal-sin 1000.0))   ;; 1000Hz baseband signal
(define carrier-sig  (signal-sin 100000.0)) ;; 100KHz carrier

(define transmit-sig (signal-prod baseband-sig carrier-sig))

(define tx-buffer (bufcreate (* 4 1024)))

(sample-signal transmit-sig 500000.0 tx-buffer)

(define rx-buffer (bufcreate (* 4 1024)))


(defun mix-down (in-buffer out-buffer) 
  (loopfor i 0 (< i 1024) (+ i 1) {
        (var time (/ i 500000.0))
        (var s    (bufget-f32 in-buffer (* i 4) 'little-endian))
        (var c    (sin (* 100000.0 two-pi time)))
        (var m    (* s c))
        (bufset-f32 out-buffer (* i 4) m 'little-endian)
        }))

(mix-down tx-buffer rx-buffer)

(defun lowpass (input-buffer output-buffer alpha) {
      (var num-samples (/ (length input-buffer) 4))
      (var prev 0.0)

      (loopfor i 0 (< i num-samples) (+ i 1) {
          (var input (bufget-f32 input-buffer (* i 4) 'little-endian))
          (setq prev (+ (* alpha input) (* (- 1.0 alpha) prev)))
          (bufset-f32 output-buffer (* i 4) prev 'little-endian)
      })
  })

(define filtered-buffer (bufcreate (* 4 1024)))

;; Demodulated signal
(lowpass rx-buffer filtered-buffer 0.1)

(with-file "wave1.bin" "wb"
           (lambda (x) (fwrite x tx-buffer)))

(with-file "wave2.bin" "wb"
           (lambda (x) (fwrite x filtered-buffer)))

(print "output: mixing1.pdf")
(plot-signal-signal "wave1.bin" "wave2.bin" "mixing1.pdf"
                    "Demodulation of DSB-SC modulated signal"
                    "Carrier * baseband"
                    "Demodulated signal")

;; Experiment: Is low-pass filtering enough to recover the base-band signal?
;; The result shows that low-pass filtering is not enough.
(lowpass tx-buffer filtered-buffer 0.01) ;; filter very strongly!

(with-file "wave1.bin" "wb"
           (lambda (x) (fwrite x tx-buffer)))

(with-file "wave2.bin" "wb"
           (lambda (x) (fwrite x filtered-buffer)))

(print "output: mixing2.pdf")
(plot-signal-signal "wave1.bin" "wave2.bin" "mixing2.pdf"
                    "Experiment with filtering"
                    "Carrier * baseband"
                    "Low-pass filtering mixed signal")



;; Experiment: downmixing the signal when the carrier
;; wave and the demodulation wave are not in phase.

(define phaseplots nil)

(define interesting-phases
    `((0.0 . "0 - Perfect phase lock")
      (,(* pi 0.25) . "45 - 0.707 amplitude")
      (,(* pi 0.5) . "90 - Complete null")
      (,(* pi 0.75) . "135° - Negative 0.707")
      (,pi . "180 - Full inversion")
      (,(* pi 1.25) . "225 - Negative 0.707")
      (,(* pi 1.5) . "270 - Complete null")
      (,(* pi 1.75) . "315 - 0.707 amplitude")))

(loopfor i 0 (< i (length interesting-phases)) (+ i 1) {
      (var psh (ix interesting-phases i)) ;; (* i (/ two-pi 5)))
      (sample-signal (signal-phase-shift transmit-sig (car psh)) 500000.0 tx-buffer)
      (mix-down tx-buffer rx-buffer)
      (lowpass rx-buffer filtered-buffer 0.1)
      (var filename (str-join (list "wave" (to-str i) ".bin")))
      (var leg filename)
      (var leg (str-join (list "Phase shift " (cdr psh)))) ;; (to-str psh) )))
      (with-file filename "wb"
        (lambda (x) (fwrite x filtered-buffer)))
      (setq phaseplots (cons `( ,filename . ,leg) phaseplots))
      })

(print "output: mixing-phases.pdf")
(plot-signals phaseplots "mixing-phases.pdf" "Out of phase mixing")

      
