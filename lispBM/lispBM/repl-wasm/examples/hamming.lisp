(import "/libs/dsp_lang.lisp" 'dsp)
(read-eval-program dsp)

(define n-samples 1024)
(define sample-rate 20000.0)

(define data-r     (bufcreate (* 4 n-samples)))
(define data-im    (bufcreate (* 4 n-samples)))
(define magnitudes (bufcreate (* 4 512)))
(bufclear data-im 0 0 (* 4 n-samples))

;; Sum of sines at 1, 2, ..., 10 kHz
(define test-sig (signal-const 0))
(loop ((i 0)) (< i 10) {
  (setq test-sig (signal-sum (signal-sin (* i 1000)) test-sig))
  (setq i (+ i 1))
})

;; Sample signal
(sample-signal-from test-sig sample-rate 0.0 data-r)

(def tab (wasm-create-tab "Hamming"))

;; Plot original waveform
(wasm-add-plot tab data-r "Sum of sines (original)")

;; Apply Hamming window in-place
(loop ((i 0)) (< i n-samples) {
  (var v (bufget-f32 data-r (* i 4) 'little-endian))
  (var w (- 0.54 (* 0.46 (cos (/ (* two-pi i) (- n-samples 1.0))))))
  (bufset-f32 data-r (* i 4) (* v w) 'little-endian)
  (setq i (+ i 1))
})

;; Plot windowed waveform
(wasm-add-plot tab data-r "Sum of sines (Hamming windowed)")

;; FFT and magnitude spectrum
(let ((fft-result (fft data-r data-im 'little-endian))) {
  (loop ((i 0)) (< i 512) {
    (var x (bufget-f32 (car fft-result) (* i 4) 'little-endian))
    (var y (bufget-f32 (cdr fft-result) (* i 4) 'little-endian))
    (bufset-f32 magnitudes (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
    (setq i (+ i 1))
  })

  (wasm-add-plot tab magnitudes "FFT Magnitude Spectrum")

  ;; Inverse FFT to verify reconstruction
  (define recreated (fft (car fft-result) (cdr fft-result) 'inverse 'little-endian))
  (wasm-add-plot tab (car recreated) "Recreated waveform (inverse FFT)")
})
