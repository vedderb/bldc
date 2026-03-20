#!/usr/bin/env -S shlbm  -H 32000 -M 512000 --

(import "dsp_lang.lisp")


;; PSK encodes data in changes of phase of a carrier wave.

(define carrier-sig (signal-sin 100000.0))

(defun sinc (x)
  (if (= x 0)
      1.0
      (/ (sin x) x)))

(defun sinc-pulse ( x x-pos symbol-time n)
  (if (> (abs (- x x-pos)) (* n symbol-time))
      0.0
      (sinc (* (/ pi symbol-time) (- x x-pos)))))


(defun signal-sinc (x-center dur a)
  (signal-fun (lambda (tim)
                    (* a (sinc-pulse tim x-center dur 2)))))

(defun signal-at (t-start sig)
  (signal-switch-after (signal-const 0.0) sig t-start))

(defun  message-sinc (msg s-time) {
        (var sig (signal-const 0))
        ;; Intentionally start a bit in the future
        (var curr-time 0.0001)
        (loopforeach b msg
              {
              (setq sig (signal-sum
                         (signal-sinc curr-time s-time (if (= b 1) 1.0 -1.0)) sig))
              (setq curr-time (+ curr-time s-time))
              })
        sig
        })



(define symbols-buf (bufcreate (* 4 1024)))

(define msg-sig (message-sinc '(1 0 1 0 1 0 1 0 1 0 1 1 0 0) 0.0001))
;(define msg-sig (message-sinc '(1) 0.0001))

(sample-signal msg-sig 500000.0 symbols-buf)


;; Plot the signal representing the symbols
(with-file "wave1.bin" "wb"
           (lambda (x) (fwrite x symbols-buf)))

(print "output: message_pulse_shaping.pdf")

(plot-signal "wave1.bin" "message_pulse_shaping.pdf"
             "Sequence of bits encoded")




;; PSK modulated carrier is created by multiplying
;; the message signal with the carrier signal.
(define psk-modulated-carrier (signal-prod msg-sig carrier-sig));

(define modulated-buf (bufcreate (* 4 1024)))

(sample-signal psk-modulated-carrier 500000.0 modulated-buf)

(with-file "wave2.bin" "wb"
           (lambda (x) (fwrite x modulated-buf)))

(print "output: psk_modulated_pulse_shaping.pdf")
(plot-signal-signal "wave1.bin" "wave2.bin" "psk_modulated_pulse_shaping.pdf"
                    "Symbol signal and modulated onto carrier"
                    "Symbols"
                    "Psk modulated carrier")



;; Lets run FFT and see if it indicates the bandwidth occupied
;; by the message

(define data-im (bufcreate (* 4 1024)))
(define magnitudes (bufcreate (* 4 512)))

(let ((fft-r (fft modulated-buf data-im 'little-endian)))
  {
  (loopfor i 0 (< i 512) (+ i 1) {
        (var x (bufget-f32 (car fft-r) (* i 4) 'little-endian))
        (var y (bufget-f32 (cdr fft-r) (* i 4) 'little-endian))
        (bufset-f32 magnitudes (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
        })
  })

(with-file "fft.bin" "wb"
           (lambda (x) (fwrite x magnitudes)))

(print "psk_modulated_spectrum_pulse_shaping.pdf")
(plot-signal-spectrum "wave2.bin" "fft.bin" "psk_modulated_spectrum_pulse_shaping.pdf" "Signal Analysis" "Input Wave" "Spectrum")
