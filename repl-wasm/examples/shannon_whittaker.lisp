(import "/libs/dsp_lang.lisp" 'dsp)
(read-eval-program dsp)

;; Example of sampling near the nyquist limit and 
;; reconstruction using the Shannon-Whittaker reconstruction.
;; The sampled signal should contain no frequencies over fs/2 (sampling rate divided by 2)
;;
;; Shannon-Whittaker reconstruction uses the samples 
;; in a buffer as the magnitude for a sinc function centered in time 
;; on that sample. All of these sinc functions are then added up over 
;; the entire sample buffer. 
;; 
;; As with many theoretical results the devil is in the details. For this 
;; to work "perfectly" one would need infinitely long buffers of samples 
;; and add up all the infinitely long sinc functions. 

(define freq    990.0)
(define fs-low  2000.0)
(define fs-high 20000.0)
(define dur     0.10)

(define n-low  (to-i (* fs-low  dur)))
(define n-high (to-i (* fs-high dur)))
(define orig   (signal-sin freq))

(define low-buf (bufcreate (* n-low 4)))
(sample-signal-from orig fs-low 0.0 low-buf)

(defun sinc (x)
  (if (= x 0.0) 1.0 (/ (sin (* pi x)) (* pi x))))

(defun sw-recon (tim) {
  (var acc 0.0)
  (loopfor i 0 (< i n-low) (+ i 1) {
    (var s (bufget-f32 low-buf (* i 4) 'little-endian))
    (setq acc (+ acc (* s (sinc (- (* tim fs-low) i)))))
  })
  acc
  })

(define recon-buf (bufcreate (* n-high 4)))
(sample-signal-from (signal-fun (lambda (tim) (sw-recon tim))) fs-high 0.0 recon-buf)

(define orig-buf (bufcreate (* n-high 4)))
(sample-signal-from orig fs-high 0.0 orig-buf)

(def tab (wasm-create-tab "Shannon-Whittaker"))
(wasm-add-plot tab low-buf   "Sampled at 2 kHz")
(wasm-add-plot tab recon-buf "Shannon-Whittaker reconstruction at 20 kHz")
(wasm-add-plot tab orig-buf  "Original 990 Hz at 20 kHz")
