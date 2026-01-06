

(rtlsdr-open 0)
(rtlsdr-set-sample-rate 1102500)
(rtlsdr-set-center-freq 107100000)
(rtlsdr-set-tuner-gain-mode 'gain-mode-auto)
(rtlsdr-set-agc-mode 'agc-on)

(rtlsdr-start-sampling)

(rtlsdr-start-fm-playback)

(defun scan-from-to (start end step time)
  (loopfor i start (< i end) (+ i step) {
           (rtlsdr-set-center-freq i)
           (print "freq: " (rtlsdr-get-center-freq))
           (print "RMS:  " (rtlsdr-signal-strength))
           (sleep time)
           }))      
           
(sdl-init)

(define win (sdl-create-window "spectrum" 1024 200))
(define rend (sdl-create-soft-renderer win))
(define disp (img-buffer 'indexed2 1024 200))


(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

(sdl-set-active-renderer rend) ;; Connect the renderer to the display library


;; mags are in fft-order 0 to +Nyquist followed by -Nyquist to 0.
;; draw_spectrum rearranges the data so that 0 is in the middle.
(defun draw_spectrum (mags m) 
  (loopfor i 0 (< i 512) (+ i 1)
           {
           (img-setpix disp i (- 200 (* 200.0 (/ (bufget-f32 mags (* (+ i 512) 4) 'little-endian) m))) 1)
           (img-setpix disp (+ i 512) (- 200 (* 200.0 (/ (bufget-f32 mags (* i 4) 'little-endian) m))) 1)
           }))

(defun the-loop () {
       (var (i_buf . q_buf)  (rtlsdr-get-samples 1024))
       (var (fft_i . fft_q)  (fft i_buf q_buf 'little-endian))
       (var max_mag 0.0);
       (loopfor i 0 (< i 1024) (+ i 1) {
                (var x (bufget-f32 fft_i (* i 4) 'little-endian))
                (var y (bufget-f32 fft_q (* i 4) 'little-endian))
                ;; reuse fft-i for mags
                (var mag (sqrt (+ (* x x) (* y y))))
                (if (> mag max_mag) (setq max_mag mag))
                (bufset-f32 fft_i (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
                })
       (img-clear disp)
       (draw_spectrum fft_i (if (<= max_mag 0.0001) 0.0001 max_mag))
       (disp-render disp 0 0  '(0x000000 0xFFFFFF))
       (the-loop)
       })

(the-loop)
