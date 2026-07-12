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
   ( (signal-noise (? a)) (* a (- (* 2.0 (/ (to-float (random)) (to-float (rand-max)))) 1.0)))
   ( (signal-const (? v)) v)
   ((signal-sum (? s1) (? s2)) (+ (eval-signal s1 sig-t) (eval-signal s2 sig-t)))
   ((signal-prod (? s1) (? s2)) (* (eval-signal s1 sig-t) (eval-signal s2 sig-t)))
   ((signal-phase-shift (? s) (? p)) (eval-signal s (+ sig-t p)))
   ((signal-switch-after (? s1) (? s2) (? t-switch))
    (if (< sig-t t-switch)
        (eval-signal s1 sig-t)
        (eval-signal s2 (- sig-t t-switch))))
   ))

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

(defun plot-signal (s sample-rate duration title)
  {
  (var num-samples (to-i (* sample-rate duration)))
  (var buf (bufcreate (* num-samples 4)))
  (sample-signal-from s sample-rate 0.0 buf)
  (var tab (wasm-create-tab title))
  (wasm-add-plot tab buf title)
  })

