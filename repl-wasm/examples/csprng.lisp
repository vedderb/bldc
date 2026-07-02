
(defun csprng16_16 (s n) {
  (var buf (bufcreate 4))
  (bufset-u16 buf 0 s)
  (bufset-u16 buf 2 n)
  (sha256 buf)
})

(defun rand16-slice (s n)
  (bufget-u16 (csprng16_16 s n) 0))

(defun rand16-sum (s n) {
  (var h (csprng16_16 s n))
  (var acc 0)
  (loop ((i 0)) (< i 16) {
    (setq acc (mod (+ acc (bufget-u16 h (* i 2))) 65536))
    (setq i (+ i 1))
  })
  acc
})

(define prng-seed   32077)
(define n-samples  10000)
(define n-bins       64)
(define bin-width (/ 65536 n-bins))  ; 1024 per bin

(define hist-slice (bufcreate (* 4 n-bins)))
(define hist-sum   (bufcreate (* 4 n-bins)))

(define sum-s  0.0) (define sum-sq-s 0.0)
(define sum-x  0.0) (define sum-sq-x 0.0)

(loop ((i 0)) (< i n-samples) {
  (var vs (to-float (rand16-slice prng-seed i)))
  (var vx (to-float (rand16-sum   prng-seed i)))
  (var bs (/ (to-i vs) bin-width))
  (var bx (/ (to-i vx) bin-width))
  (bufset-f32 hist-slice (* bs 4)
    (+ (bufget-f32 hist-slice (* bs 4) 'little-endian) 1.0) 'little-endian)
  (bufset-f32 hist-sum (* bx 4)
    (+ (bufget-f32 hist-sum (* bx 4) 'little-endian) 1.0) 'little-endian)
  (setq sum-s    (+ sum-s  vs))
  (setq sum-sq-s (+ sum-sq-s (* vs vs)))
  (setq sum-x    (+ sum-x  vx))
  (setq sum-sq-x (+ sum-sq-x (* vx vx)))
  (setq i (+ i 1))
})

(def tab (wasm-create-tab "CSPRNG"))
(wasm-add-plot-multi tab (list hist-slice hist-sum)
                     (str-join (list "Histogram: rand16-slice vs rand16-sum (" (str-from-n n-samples) " samples, 64 bins)")))

(define n (to-float n-samples))
(define mean-s  (/ sum-s  n))
(define mean-x  (/ sum-x  n))
(define var-s   (- (/ sum-sq-s n) (* mean-s mean-s)))
(define var-x   (- (/ sum-sq-x n) (* mean-x mean-x)))

(define expected (/ n (to-float n-bins)))
(define chi2-s 0.0)
(define chi2-x 0.0)
(loop ((i 0)) (< i n-bins) {
  (var obs-s (bufget-f32 hist-slice (* i 4) 'little-endian))
  (var obs-x (bufget-f32 hist-sum   (* i 4) 'little-endian))
  (var ds (- obs-s expected))
  (var dx (- obs-x expected))
  (setq chi2-s (+ chi2-s (/ (* ds ds) expected)))
  (setq chi2-x (+ chi2-x (/ (* dx dx) expected)))
  (setq i (+ i 1))
})

(print "rand16-slice")
(print (str-join (list "  mean:     " (to-str mean-s) "  (ideal: 32767.5)")))
(print (str-join (list "  variance: " (to-str var-s)  "  (ideal: 357913941)")))
(print (str-join (list "  chi2:     " (to-str chi2-s) "  (63 degrees of freedom, ideal ~63)")))

(print "rand16-sum")
(print (str-join (list "  mean:     " (to-str mean-x) "  (ideal: 32767.5)")))
(print (str-join (list "  variance: " (to-str var-x)  "  (ideal: 357913941)")))
(print (str-join (list "  chi2:     " (to-str chi2-x) "  (63 degrees of freedom, ideal ~63)")))
