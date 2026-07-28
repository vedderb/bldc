;; 2-Antenna Array Direction Finding
;; Adapted from repl/examples/dsp/dsp_angle_detection.lisp

(import "/libs/dsp_lang.lisp" 'dsp)
(read-eval-program dsp)

;; Parameters
(define signal-freq      2000.0)   ; Hz
(define sample-rate     20000.0)   ; Hz
(define arrival-angle-deg  30.0)   ; degrees from broadside
(define arrival-angle-rad (* arrival-angle-deg (/ pi 180.0)))
(define true-phase-diff (* pi (sin arrival-angle-rad)))
(define signal-bin 102)            ; bin = 2000 * 1024 / 20000
(define n-samples 1024)

;; Buffers for antenna signals
(define ant0-buf (bufcreate (* 4 n-samples)))
(define ant1-buf (bufcreate (* 4 n-samples)))
(define ant0-im  (bufcreate (* 4 n-samples)))
(define ant1-im  (bufcreate (* 4 n-samples)))

;; Sample signals at both antennas (ant1 has extra phase shift from angle)
(sample-signal-from (signal-sin signal-freq)                 sample-rate 0.0 ant0-buf)
(sample-signal-from (signal-sin signal-freq true-phase-diff) sample-rate 0.0 ant1-buf)

(def tab (wasm-create-tab "Direction Finding"))

;; Plot both antenna signals
(wasm-add-plot-multi tab (list ant0-buf ant1-buf) "Antenna Signals (ant0 and ant1)")

;; Extract phase at a frequency bin, averaging nearby bins to handle leakage
(defun extract-phase (fft-real fft-imag peak-bin) {
  (var sum-r 0.0)
  (var sum-i 0.0)
  (loop ((j (- peak-bin 2))) (<= j (+ peak-bin 2)) {
    (setq sum-r (+ sum-r (bufget-f32 fft-real (* j 4) 'little-endian)))
    (setq sum-i (+ sum-i (bufget-f32 fft-imag (* j 4) 'little-endian)))
    (setq j (+ j 1))
  })
  (atan2 sum-i sum-r)
})

(defun wrap-phase (diff) {
  (if (> diff pi)
      (- diff two-pi)
      (if (< diff (- 0 pi))
          (+ diff two-pi)
          diff))
})

;; Detect angle from a single simulation
(defun detect-angle (angle-deg) {
  (var angle-rad (* angle-deg (/ pi 180.0)))
  (var phase-diff (* pi (sin angle-rad)))
  (sample-signal-from (signal-sin signal-freq)            sample-rate 0.0 ant0-buf)
  (sample-signal-from (signal-sin signal-freq phase-diff) sample-rate 0.0 ant1-buf)
  (bufclear ant0-im 0 0 (* 4 n-samples))
  (bufclear ant1-im 0 0 (* 4 n-samples))
  (var fft-a (fft ant0-buf ant0-im 'little-endian))
  (var fft-b (fft ant1-buf ant1-im 'little-endian))
  (var p0 (extract-phase (car fft-a) (cdr fft-a) signal-bin))
  (var p1 (extract-phase (car fft-b) (cdr fft-b) signal-bin))
  (* (asin (/ (wrap-phase (- p1 p0)) pi)) (/ 180.0 pi))
})

;; Sweep from -80 to +80 degrees in 5 degree steps (33 points)
(define n-angles 33)
(define true-angles-buf      (bufcreate (* 4 n-angles)))
(define recovered-angles-buf (bufcreate (* 4 n-angles)))

(loop ((i 0) (angle -80.0)) (< i n-angles) {
  (bufset-f32 true-angles-buf      (* i 4) angle                  'little-endian)
  (bufset-f32 recovered-angles-buf (* i 4) (detect-angle angle)   'little-endian)
  (setq i     (+ i 1))
  (setq angle (+ angle 5.0))
})

;; Plot true vs recovered angle (ideal result is a straight line y=x)
(wasm-add-plot-xy tab true-angles-buf recovered-angles-buf "Direction Finding: True vs Recovered Angle")
