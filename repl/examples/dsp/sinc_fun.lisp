#!/usr/bin/env -S shlbm -M 512000 --

(import "dsp_lang.lisp")


(define samplerate 20000)
(define window-time (/ 1024.0 samplerate))

(print "Window time " window-time)

(define symbol-time (/ window-time 10.0))

(defun sinc (x)
  (if (= x 0)
      1.0
      (/ (sin x) x)))


(define scale-to-window (/ 1.0 window-time))
(define scale-to-symbol (/ 1.0 symbol-time))

(define nyquist-criterion-scaling (/ pi symbol-time))

;; center sinc function in the sampling window
(let ((buffer (bufcreate (* 4 1024))))
  {
  (sample-signal (signal-fun
                  (lambda (x)
                    (sinc (* nyquist-criterion-scaling
                             (- x (* 0.5 window-time))))))  samplerate buffer)
  (with-file "wave.bin" "wb"
             (lambda (x) (fwrite x buffer)))
  (plot-signal "wave.bin" "sinc1.pdf"
               "sinc function")
  })

(define plot-files nil)

(define buffer (bufcreate (* 4 1024)))

(loopfor i 0 (< i 10) (+ i 1)
      {
      (var filename (str-join `("wave" ,(to-str i) ".bin")))
      (var leg filename)
      (sample-signal (signal-fun (lambda (x) (sinc (* nyquist-criterion-scaling (- x (* i symbol-time))))))  samplerate buffer)
      (with-file filename "wb"
                 (lambda (x) (fwrite x buffer)))
      (setq plot-files (cons `( ,filename . ,leg) plot-files))
      })

(plot-signals plot-files "sinc_overlapping.pdf" "sinc functions overlapping")
