#!/usr/bin/env -S shlbm -M 512000 --

;; Uses up a bit of memory, launch with.
;; ../repl -M 11 -s dsp_lang.lisp

(define pi 3.14159)
(define two-pi (* 2 pi))

(defun get-with-default (xs ys)
  (if xs
      {
      (var r nil)
      (loopfor i 0 (< i (length ys)) (+ i 1)
            (setq r (cons (if (ix xs i) (ix xs i) (ix ys i)) r))
            )
      (reverse r)
      }
      ys)
  )


;; Constructors for Deeply embedded time dependent signals
(defun signal-sin (f)
  (let (((p a) (get-with-default (rest-args) (list 0.0 1.0))))
        (list 'signal-sin f p a)
        ))
(defun signal-cos (f)
  (let ( ((p a) (get-with-default (rest-args) (list 0.0 1.0))))
    (list 'signal-cos f p a)
    ))

(defun signal-noise ()
  (let (( (a) (get-with-default (rest-args) (list 1.0))))
    (list 'signal-noise a)))

(defun signal-const (v)
  (list 'signal-const v))


;; Signal operators
(defun signal-sum (s1 s2)
  (list 'signal-sum s1 s2))
(defun signal-prod (s1 s2)
  (list 'signal-prod s1 s2))
(defun signal-switch-after (s1 s2 switch-time)
  (list 'signal-switch-after s1 s2 switch-time))

(defun signal-phase-shift (s p)
  (list 'signal-phase-shift s p))

(defun signal-fun (fun)
  (let (( (a) (get-with-default (rest-args) (list "Custom signal"))))
    (list 'signal-fun a fun)))

(defun signal-sin-chirp-linear (f0 f1 duration)
  (signal-fun
    (lambda (time)
      (if (< time duration)
          (let ((k (/ (- f1 f0) duration)))
            (sin (* two-pi (+ (* f0 time) (* 0.5 k time time)))))
          (sin (* two-pi f1 time))))
    (str-join `("Linear chirp " ,(to-str f0) "Hz -> " ,(to-str f1) "Hz over " ,(to-str duration) "s"))))


;; evaluate a signal at time t
(defun eval-signal (s sig-t)
  (match s
   ( (signal-fun _ (? sig-fun)) (sig-fun sig-t))
         
   ( (signal-sin (? f) (? p) (? a)) (* a (sin (+ (* f two-pi sig-t) p))))
   ( (signal-cos (? f) (? p) (? a)) (* a (cos (+ (* f two-pi sig-t) p))))
   ( (signal-noise (? a)) (* a (/ (to-float (random)) (rand-max))))
   ( (signal-const (? v)) v)
   ((signal-sum (? s1) (? s2)) (+ (eval-signal s1 sig-t) (eval-signal s2 sig-t)))
   ((signal-prod (? s1) (? s2)) (* (eval-signal s1 sig-t) (eval-signal s2 sig-t)))
   ((signal-phase-shift (? s) (? p)) (eval-signal s (+ sig-t p)))
   ((signal-switch-after (? s1) (? s2) (? t-switch))
    (if (< sig-t t-switch)
        (eval-signal s1 sig-t)
        (eval-signal s2 (- sig-t t-switch))))
   ))


(defun sample-signal (s sample-rate buffer ) {
       (var num-samples (/ (length buffer) 4))
       (var time-delta (/ 1.0 sample-rate))
       (var sig-t 0.0)
       (loopfor i 0 (< i num-samples) (+ i 1) {
             (bufset-f32 buffer (* i 4) (eval-signal s sig-t) 'little-endian)
             (setq sig-t (+ sig-t time-delta))
             })
       buffer
       })

(defun sample-signal-from (s sample-rate t-start buffer ) {
       (var num-samples (/ (length buffer) 4))
       (var time-delta (/ 1.0 sample-rate))
       (var sig-t t-start)
       (loopfor i 0 (< i num-samples) (+ i 1) {
             (bufset-f32 buffer (* i 4) (eval-signal s sig-t) 'little-endian)
             (setq sig-t (+ sig-t time-delta))
             })
       buffer
       })



;; Filehandling "bracket style" operation
(defun with-file (filename mode operation) {
       (var fh (fopen filename mode))
       (operation fh)
       (fclose fh)
       })


(defun plot-signal (infile outfile title)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile  "'")))
  (gnuplot-cmd gp (str-join `("set title '" ,title "'")))
  (gnuplot-cmd gp "set xlabel 'Sample Number'")
  (gnuplot-cmd gp "set ylabel 'Amplitude'")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,infile "' binary array=1024 format='%float' with lines title 'Waveform'")))
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

(defun plot-spectrum (infile outfile title)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile "'")))
  (gnuplot-cmd gp (str-join `("set title '" ,title "'")))
  (gnuplot-cmd gp "set xlabel 'Frequency Bin'")
  (gnuplot-cmd gp "set ylabel 'Magnitude'")
  (gnuplot-cmd gp (str-join `("plot '" ,infile "' binary array=512 format='%float' with lines title 'Frequency Domain'")))
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

(defun plot-magnitude-phase (mag-file phase-file outfile title)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf size 10,8")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile "'")))
  (gnuplot-cmd gp (str-join `("set multiplot layout 2,1 title '" ,title "'")))

  ;; Magnitude plot
  (gnuplot-cmd gp "set title 'Magnitude Spectrum'")
  (gnuplot-cmd gp "set xlabel 'Frequency Bin'")
  (gnuplot-cmd gp "set ylabel 'Magnitude'")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,mag-file "' binary array=512 format='%float' with lines lw 2 title 'Magnitude'")))

  ;; Phase plot
  (gnuplot-cmd gp "set title 'Phase Spectrum'")
  (gnuplot-cmd gp "set xlabel 'Frequency Bin'")
  (gnuplot-cmd gp "set ylabel 'Phase (degrees)'")
  (gnuplot-cmd gp "set yrange [-200:200]")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,phase-file "' binary array=512 format='%float' with points pt 7 ps 0.5 title 'Phase'")))

  (gnuplot-cmd gp "unset multiplot")
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

(defun plot-signal-spectrum (signal-file mag-file outfile t0 t1 t2)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf size 10,8")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile "'")))

  (gnuplot-cmd gp (str-join `("set multiplot layout 2,1 title '" ,t0 "'")))

  (gnuplot-cmd gp (str-join `("set title '" ,t1 "'")))
  (gnuplot-cmd gp "set xlabel 'Sample'")
  (gnuplot-cmd gp "set ylabel 'Amplitude'")
  (gnuplot-cmd gp (str-join `("plot '" ,signal-file "' binary array=1024 format='%float' with lines title 'Time Domain'")))

  (gnuplot-cmd gp (str-join `("set title '" ,t2 "'")))
  (gnuplot-cmd gp "set xlabel 'Frequency Bin'")
  (gnuplot-cmd gp "set ylabel 'Magnitude'")
  (gnuplot-cmd gp (str-join `("plot '" ,mag-file "' binary array=512 format='%float' with lines title 'Frequency Domain'")))

  (gnuplot-cmd gp "unset multiplot")
  (gnuplot-close gp)
  })

;; TODO: ylabels
(defun plot-signal-signal (signal1-file signal2-file outfile t0 t1 t2)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf size 10,8")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile "'")))

  (gnuplot-cmd gp (str-join `("set multiplot layout 2,1 title '" ,t0 "'")))

  (gnuplot-cmd gp (str-join `("set title '" ,t1 "'")))
  (gnuplot-cmd gp "set xlabel 'Sample'")
  (gnuplot-cmd gp "set ylabel 'Amplitude'")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,signal1-file "' binary array=1024 format='%float' with lines title '" ,t1 "'")))

  (gnuplot-cmd gp (str-join `("set title '" ,t2 "'")))
  (gnuplot-cmd gp "set xlabel 'Sample'")
  (gnuplot-cmd gp "set ylabel 'Amplitude'")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,signal2-file "' binary array=1024 format='%float' with lines title '" ,t2 "'")))

  (gnuplot-cmd gp "unset multiplot")
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

(defun plot-signals (file-legend-pairs outfile title)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf size 10,6")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile "'")))
  (gnuplot-cmd gp (str-join `("set title '" ,title "'")))
  (gnuplot-cmd gp "set xlabel 'Sample'")
  (gnuplot-cmd gp "set ylabel 'Amplitude'")
  (gnuplot-cmd gp "set grid")

  (var plot-cmd "plot ")
  (var first-elt t)
  (loopfor i 0 (< i (length file-legend-pairs)) (+ i 1) {
    (var pair (ix file-legend-pairs i))
    (var file (car pair))
    (var legend (cdr pair))

    (if (not first-elt)
        (setq plot-cmd (str-join `(,plot-cmd ", "))))
    (setq first-elt nil)

    (setq plot-cmd (str-join (list plot-cmd "'" file "' binary array=1024 format='%float' with lines title '" legend "'")))
  })

  (gnuplot-cmd gp plot-cmd)
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

;; TODOs:
;; - Windows and FFT (Hann, Hamming, Blackman - compare side lobe reduction)
;; - Peak detection (req. mean, std_dev)
;; - Examples illustrating dB (linear vs log magnitude plots)
;; - Spectrogram (time-frequency visualization using sliding window FFT)
;; - Frequency interpolation (parabolic/quadratic peak fitting for sub-bin accuracy)
;; - Zero-padding FFT (increase frequency resolution artificially)
;; - Inverse FFT (reconstruct time-domain signal from frequency domain)
;; - Filter design (low-pass, high-pass, band-pass using frequency domain multiplication)
;; - Cross-correlation and auto-correlation
;; - Frequency modulation (FM synthesis)
;; - Amplitude modulation (AM) and demodulation
;; - Aliasing demonstration (sample signals above Nyquist frequency)
;; - Phase unwrapping (remove discontinuities in phase plots)
;; - Compare different FFT sizes (trade-off between freq/time resolution)
