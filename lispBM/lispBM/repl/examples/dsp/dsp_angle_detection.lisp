#!/usr/bin/env -S shlbm -H 32000 -M 512000 --

(import "dsp_lang.lisp")

;; Experiment where 2 antenna pick up a signal

;; a_x antenna
;;
;;  |||||||||||||||||||||||||||||
;;  |||||||||||||||||||||||||||||  signal straight on
;;  |||||||||||||||||||||||||||||
;;  |||||||||||||||||||||||||||||
;;   a_1                      a_2
;; wavefront reaches a_1 and a_2 at the same time => no difference in relative phase.
;;

;;  \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;;  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\     signal at angle
;;  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;;   a_1                      a_2
;; wavefront reaches a_1 before a_2 => there is a relative difference in phase.

;;     | \
;;     | A \       There is an angle A here that we do not know!
;;     |     \
;;    a_1 -- a_2

;; We don't know the Angle A, but we do know these things:
;;
;; d = distance(a_1, a_2)
;; relative phase shift s (delta phase): by doing the FFT and the phase shift example above.
;; frequency f, wavelength l (either we know what we are looking for, or we look at the spectrum for peaks)
;;
;; difference in travel distance is: d * sin(A)
;;
;;                  0 deg
;;                  |
;;                  |
;;                  |
;;    90deg ----------------- -90deg
;;             a_1      a_2
;;
;; A signal coming in from 90 degrees, will hit a_1 first then travel exactly d distance before it reaches a_2
;;   sin(90) = 1    and d * sin(90) = d
;;
;; A signal coming in from -90 degrees will hit a_2 after traveling -d distance from a_1 ...
;;   sin(-90) = -1 and d * sin(-90) = -d
;;
;; A signal coming in from 0 degrees has hits a_2 and a_1 at the same tine (no additional distance)
;;   sin(0) = 0 and d * sin(0) = 0

;; Between angles 90 to 0 sin range from 1 to 0 and between angles 0 to -90 sin range from 0 to -1
;; signals coming in at angles between 90 and 0 degrees (and 0 and -90 likewise) travel some portion
;; of the distance d extra and and for these angles sin ranges from 1 to 0 (and 0 to -1) smoothly.
;;
;;                    P/ 90deg angle here where projection of a_1 meets signal
;;              |     /\
;;              |   /   \ path-diff: p -> a_2
;;              | /      \
;;           ---/--------------             sin(A) = opposite / hypotenuse
;;            /a_1        a_2               d = distance(a_1, a_2) = hypotenuse
;;              |---- d ---|                sin(A) = path-diff / d
;;                                          d * sin(A) = path-diff
;;

;; difference in travel distance is: d * sin(A)
;; difference in travel distance is also: s * l / (2 * pi)      [l / 2pi : distance per radian, s : number of radians]
;;
;;
;; combine info from the two formulas: sin(A) = ((s * l / (2 * pi)) / d) = (s * l / (2 * pi * d))
;; and then A = asin(s * l / (2 * pi * d))
;;
;; d is up to us to select, in the example below d = l/2
;;  A = asin(s * l / (2 * pi * d))
;;    = asin(s * l / (2 * pi * (l/2)))
;;    = asin(s * l / (pi * l))
;;    = asin(s / pi);
;;
;;  So in the end, all we need is the shift and we get the angle!

(print "--- 2-Antenna Array Direction Finding ---")

;; Parameters
(define signal-freq 2000.0)        ; Hz
(define sample-rate 20000.0)       ; Hz
(define arrival-angle-deg 30.0)    ; degrees from broadside
(define arrival-angle-rad (* arrival-angle-deg (/ pi 180.0)))

;; With d = lambda/2, the phase difference is simply pi*sin(theta)
(define true-phase-diff (* pi (sin arrival-angle-rad)))

(print "True arrival angle: " arrival-angle-deg " degrees")
(print "Expected phase difference: " true-phase-diff " radians")

;; Create signals at each antenna
;; Ant0 receives: sin(2*pi*f*t + phi0)         where phi0 is arbitrary (sampling start)
;; Ant1 receives: sin(2*pi*f*t + phi0 + dphi)  same phi0, but extra path delay
;;
;; We model this by giving Ant1's signal an additional phase offset

(define ant0-signal (signal-sin signal-freq))
(define ant1-signal (signal-sin signal-freq true-phase-diff))

;; Sample both antennas (same time window, same t0)
(define ant0-buf (bufcreate (* 4 1024)))
(define ant1-buf (bufcreate (* 4 1024)))

(sample-signal ant0-signal sample-rate ant0-buf)
(sample-signal ant1-signal sample-rate ant1-buf)

;; FFT both channels
(define ant0-im (bufcreate (* 4 1024)))
(define ant1-im (bufcreate (* 4 1024)))

(define fft0 (fft ant0-buf ant0-im 'little-endian))
(define fft1 (fft ant1-buf ant1-im 'little-endian))

(define fft0-r (car fft0))
(define fft0-i (cdr fft0))
(define fft1-r (car fft1))
(define fft1-i (cdr fft1))

;; Find the bin for our signal frequency
;; bin = freq * N / sample_rate = 2000 * 1024 / 20000 = 102.4
(define signal-bin 102)

;; Extract phase at signal bin for both antennas
;; Average over nearby bins to handle spectral leakage
(defun extract-phase (fft-real fft-imag peak-bin) {
      (var sum-r 0.0)
      (var sum-i 0.0)
      (loopfor j (- peak-bin 2) (<= j (+ peak-bin 2)) (+ j 1) {
            (setq sum-r (+ sum-r (bufget-f32 fft-real (* j 4) 'little-endian)))
            (setq sum-i (+ sum-i (bufget-f32 fft-imag (* j 4) 'little-endian)))
            })
      (atan2 sum-i sum-r)
      })

(define phase0 (extract-phase fft0-r fft0-i signal-bin))
(define phase1 (extract-phase fft1-r fft1-i signal-bin))

(print "Ant0 phase: " phase0 " rad")
(print "Ant1 phase: " phase1 " rad")

;; Compute phase difference
(define measured-phase-diff (- phase1 phase0))

;; Handle phase wrapping (keep in -pi to pi range)
(define measured-phase-diff
  (if (> measured-phase-diff pi)
      (- measured-phase-diff two-pi)
      (if (< measured-phase-diff (- 0 pi))
          (+ measured-phase-diff two-pi)
          measured-phase-diff)))

(print "Measured phase difference: " measured-phase-diff " rad")

(define recovered-angle-rad (asin (/ measured-phase-diff pi)))
(define recovered-angle-deg (* recovered-angle-rad (/ 180.0 pi)))

(print "Recovered arrival angle: " recovered-angle-deg " degrees")
(print "Error: " (- recovered-angle-deg arrival-angle-deg) " degrees")

;; Sweep angle: simulate source moving parallel to array

(print "--- Angle Sweep: Source Moving Parallel to Array ---")

(defun detect-angle (angle-deg) {
      ;; Compute true phase difference for this angle
      (var angle-rad (* angle-deg (/ pi 180.0)))
      (var phase-diff (* pi (sin angle-rad)))

      ;; Create signals at each antenna
      (var sig0 (signal-sin signal-freq))
      (var sig1 (signal-sin signal-freq phase-diff))

      (sample-signal sig0 sample-rate ant0-buf)
      (sample-signal sig1 sample-rate ant1-buf)

      (bufclear ant0-im 0)
      (bufclear ant1-im 0)

      (var fft-a (fft ant0-buf ant0-im 'little-endian))
      (var fft-b (fft ant1-buf ant1-im 'little-endian))

      ;; Extract phases and compute difference
      (var p0 (extract-phase (car fft-a) (cdr fft-a) signal-bin))
      (var p1 (extract-phase (car fft-b) (cdr fft-b) signal-bin))
      (var measured-diff (- p1 p0))

      ;; Wrap to -pi..pi
      (var wrapped-diff
        (if (> measured-diff pi)
            (- measured-diff two-pi)
            (if (< measured-diff (- 0 pi))
                (+ measured-diff two-pi)
                measured-diff)))

      ;; Recover angle
      (* (asin (/ wrapped-diff pi)) (/ 180.0 pi))
      })

;; Sweep from -80 to +80 degrees in 5 degree steps
(define angle-data-buf (bufcreate (* 4 33 2)))  ; 33 points, 2 floats each (true, recovered)

(define idx 0)
(loopfor angle -80.0 (<= angle 80.0) (+ angle 5.0) {
      (var r(detect-angle angle))
      (bufset-f32 angle-data-buf (* idx 4) angle 'little-endian)
      (bufset-f32 angle-data-buf (+ (* idx 4) (* 33 4)) r 'little-endian)
      (setq idx (+ idx 1))
      })

(define angle-pairs-buf (bufcreate (* 4 2 33)))

(loopfor i 0 (< i 33) (+ i 1) {
      (var true-ang (bufget-f32 angle-data-buf (* i 4) 'little-endian))
      (var recv-ang (bufget-f32 angle-data-buf (+ (* i 4) (* 33 4)) 'little-endian))
      (bufset-f32 angle-pairs-buf (* i 8) true-ang 'little-endian)
      (bufset-f32 angle-pairs-buf (+ (* i 8) 4) recv-ang 'little-endian)
      })

(define f-pairs (fopen "angle_pairs.bin" "wb"))
(fwrite f-pairs angle-pairs-buf)
(fclose f-pairs)

(define gp (gnuplot-open))
(gnuplot-cmd gp "set terminal pdf")
(gnuplot-cmd gp "set output 'antenna_angle_sweep.pdf'")
(gnuplot-cmd gp "set title '2-Antenna Direction Finding: Angle Sweep'")
(gnuplot-cmd gp "set xlabel 'True Arrival Angle (degrees)'")
(gnuplot-cmd gp "set ylabel 'Recovered Angle (degrees)'")
(gnuplot-cmd gp "set xrange [-90:90]")
(gnuplot-cmd gp "set yrange [-90:90]")
(gnuplot-cmd gp "set grid")
(gnuplot-cmd gp "set key top left")

(gnuplot-cmd gp "plot x title 'Ideal (y=x)' with lines, 'angle_pairs.bin' binary record=33 format='%float%float' using 1:2 with points  title 'Recovered'")
(gnuplot-cmd gp "set output")
(gnuplot-close gp)

(print "output: antenna_angle_sweep.pdf")
