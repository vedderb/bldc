#!/usr/bin/env -S shlbm -H 32000 -M 512000 --

(import "dsp_lang.lisp")

;; The mixing.lisp example shows a problem with 
;; downmixing. The phase information is lost.
;; Depending on relative phase of the carrier and the
;; downmixing signal, we may or may not retrieve the modulated
;; signal. Only if the downmixing signal is phase synced with
;; carrier do we recover the correct phase of the modulated signal.
;;
;; There is an IQ mixing technique that amends the above mentioned
;; problems.

(define baseband-sig (signal-sin 1000.0))   ;; 1000Hz baseband signal
(define carrier-sig  (signal-sin 100000.0)) ;; 100KHz carrier

(define transmit-sig (signal-prod baseband-sig carrier-sig))

;; transmit-sig  = sin(2pi * 1000 * t) * sin(2pi * 100000 * t)
;;
;; Using identity: sin(A)*sin(B) = (1/2)[cos(A-B) - cos(A+B)]
;;
;; transmit-sig = (1/2)(cos((2pi * 1000 * t) - (2pi * 100000 * t)) -
;;                      cos((2pi * 1000 * t) + (2pi * 100000 * t)))
;;              = (1/2)(cos(2pi * t * (1000 - 100000)) -
;;                      cos(2pi * t * (1000 + 100000)))
;;              = (1/2)(cos(2pi * t * -99000) -
;;                      cos(2pi * t * 101000))
;;              = (1/2)(cos(2pi * t * 99000) -   :: [cos(-x) = cos(x)] 
;;                      cos(2pi * t * 101000))
;; Just like signal-sum a subtracting a signal from another
;; results in the same kind of signal superposition
;;
;; The result is two frequency components at 99kHz and 101kHz
;;         (carrier - baseband) and (carrier + baseband)
;;


(defun iq-mix-down (in-buffer i-buffer q-buffer) 
  (loopfor i 0 (< i 1024) (+ i 1) {
        (var time (/ i 500000.0))
        (var s    (bufget-f32 in-buffer (* i 4) 'little-endian))
        (var c0   (cos (* 100000.0 two-pi time)))
        (var c1   (sin (* 100000.0 two-pi time))) 
        (var mi   (* s c0))
        (var mq   (* s c1))
        (bufset-f32 i-buffer (* i 4) mi 'little-endian)
        (bufset-f32 q-buffer (* i 4) mq 'little-endian)
        }))

(defun lowpass (input-buffer output-buffer alpha) {
      (var num-samples (/ (length input-buffer) 4))
      (var prev 0.0)

      (loopfor i 0 (< i num-samples) (+ i 1) {
          (var input (bufget-f32 input-buffer (* i 4) 'little-endian))
          (setq prev (+ (* alpha input) (* (- 1.0 alpha) prev)))
          (bufset-f32 output-buffer (* i 4) prev 'little-endian)
      })
  })

(defun mag (i1-buf i2-buf output-buffer) {
       (var num-samples (/ (length i1-buf) 4))
       ;; i2-buf must be same length
       (loopfor i 0 (< i num-samples) (+ i 1) {
             (var a (bufget-f32 i1-buf (* i 4) 'little-endian))
             (var b (bufget-f32 i2-buf (* i 4) 'little-endian))
             (bufset-f32 output-buffer (* i 4) (sqrt (+ (* a a) (* b b)))  'little-endian)
             })
       })




(define tx-buffer (bufcreate (* 4 1024)))

(sample-signal transmit-sig 500000.0 tx-buffer)

(define i-buffer (bufcreate (* 4 1024)))
(define q-buffer (bufcreate (* 4 1024)))

(iq-mix-down tx-buffer i-buffer q-buffer)

(define li-buffer (bufcreate (* 4 1024)))
(define lq-buffer (bufcreate (* 4 1024)))

(lowpass i-buffer li-buffer 0.01)
(lowpass q-buffer lq-buffer 0.01)

(define mag-buffer (bufcreate (* 4 1024)))

(mag li-buffer lq-buffer mag-buffer)

(with-file "wave1.bin" "wb"
           (lambda (x) (fwrite x tx-buffer)))

(with-file "wave2.bin" "wb"
           (lambda (x) (fwrite x mag-buffer)))

(print "output: mixing_iq1.pdf")
(plot-signal-signal "wave1.bin" "wave2.bin" "mixing_iq1.pdf"
                    "IQ Demodulation of DSB-SC modulated signal"
                    "Carrier * baseband"
                    "IQ Demodulated signal")

(with-file "i_channel.bin" "wb"
           (lambda (x) (fwrite x li-buffer)))

(with-file "q_channel.bin" "wb"
           (lambda (x) (fwrite x lq-buffer)))

(print "output: iq_channels.pdf")
(plot-signals '(("i_channel.bin" . "I channel (in-phase)")
                ("q_channel.bin" . "Q channel (quadrature)"))
              "iq_channels.pdf"
              "IQ Demodulation: I and Q Channels")

(define phase-buffer (bufcreate (* 4 1024)))

(loopfor i 0 (< i 1024) (+ i 1) {
    (var i-val (bufget-f32 li-buffer (* i 4) 'little-endian))
    (var q-val (bufget-f32 lq-buffer (* i 4) 'little-endian))
    (var phase-rad (atan2 q-val i-val))
    (bufset-f32 phase-buffer (* i 4) phase-rad 'little-endian)
})

(with-file "magnitude.bin" "wb"
           (lambda (x) (fwrite x mag-buffer)))

(with-file "phase.bin" "wb"
           (lambda (x) (fwrite x phase-buffer)))

(print "output: magnitude_phase.pdf")
(plot-signal-signal "magnitude.bin" "phase.bin" "magnitude_phase.pdf"
                    "Magnitude and phase information"
                    "Magnitude"
                    "Phase")

;; Experiment: IQ demodulation with different phase shifts
;; I am not quite understanding these things.. a 3 blue one brown is needed!

(define interesting-phases
    `((0.0 . "0 - Perfect phase lock")
      (,(* pi 0.25) . "45 - 0.707 amplitude")
      (,(* pi 0.5) . "90 - Complete null")
      (,(* pi 0.75) . "135 - Negative 0.707")
      (,pi . "180 - Full inversion")
      (,(* pi 1.25) . "225 - Negative 0.707")
      (,(* pi 1.5) . "270 - Complete null")
      (,(* pi 1.75) . "315 - 0.707 amplitude")))

(define phaseplots nil)

(define recovered-buffer (bufcreate (* 4 1024)))

(loopfor j 0 (< j (length interesting-phases)) (+ j 1) {
      (var phase-info (ix interesting-phases j))
      (var psh (car phase-info))
      (var description (cdr phase-info))

      (sample-signal (signal-phase-shift transmit-sig psh) 500000.0 tx-buffer)
      (iq-mix-down tx-buffer i-buffer q-buffer)
      (lowpass i-buffer li-buffer 0.01)
      (lowpass q-buffer lq-buffer 0.01)


      ;; Find the buffer to used based on "strngth"
      ;; This feels strange to me... 
      (var i-power 0.0)
      (var q-power 0.0)
      (loopfor i 0 (< i 100) (+ i 1) { 
          (var i-val (bufget-f32 li-buffer (* i 4) 'little-endian))
          (var q-val (bufget-f32 lq-buffer (* i 4) 'little-endian))
          (setq i-power (+ i-power (* i-val i-val)))
          (setq q-power (+ q-power (* q-val q-val)))
      })

      (var stronger-buffer (if (> i-power q-power) li-buffer lq-buffer))

      (var filename (str-join (list "iq_wave" (to-str j) ".bin")))
      (with-file filename "wb"
        (lambda (x) (fwrite x stronger-buffer)))
      (setq phaseplots (cons `(,filename . ,description) phaseplots))
})

(print "output: mixing_iq_phases.pdf")
(plot-signals phaseplots "mixing_iq_phases.pdf" "IQ Demodulation: Phase Independence")

