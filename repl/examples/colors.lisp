;; ./repl-M 11 -H 16000


(defun degtorad (d)
  (/ (* d 3.141) 180.0))

(defun rotate-point (p angle)
  `(,(- (* (car p) (cos (degtorad angle)))
        (* (cdr p) (sin (degtorad angle))))
    .
    ,(+ (* (car p) (sin (degtorad angle)))
        (* (cdr p) (cos (degtorad angle))))))


(defun trans (p v)
  `(,(+ (car p) (car v)) . ,(+ (cdr p) (cdr v))))

(defun move (p ang s)
  (let ((v (rotate-point `( 0 . ,(- s)) ang)))
    (trans p v)))

(defun line (img p1 p2)
  (img-line img (car p1) (cdr p1) (car p2) (cdr p2) 1))


(defun draw-figure (tgt img p ang s)
  (if (<= s 1)
      ()
    (let ((p1 (move p ang s))
          (p2 (move p (+ ang 60) s))
          (p3 (move p (+ ang 120) s))
          (p4 (move p (+ ang 180) s))
          (p5 (move p (+ ang 240) s))
          (p6 (move p (+ ang 300) s)))
      (progn
        (yield 100)
        (line img p p1)
        (line img p p2)
        (line img p p3)
        (line img p p4)
        (line img p p5)
        (line img p p6)
        (img-blit tgt img 96 36 -1)
        (disp-render tgt 0 0 '(0x000000 0xFFFFFF))
        (sleep 0.01)
        (draw-figure tgt img p1 (+ ang 10) (/ s 2))
        (draw-figure tgt img p2 (+ ang 70) (/ s 2))
        (draw-figure tgt img p3 (+ ang 130) (/ s 2))
        (draw-figure tgt img p4 (+ ang 190) (/ s 2))
        (draw-figure tgt img p5 (+ ang 250) (/ s 2))
        (draw-figure tgt img p6 (+ ang 310) (/ s 2))))))


(defun start-figure (tgt img p ang s)
    (draw-figure tgt img p (+ ang ) s))



(sdl-init)

(define win (sdl-create-window "LispBM XMAS" 320 200))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

(sdl-set-active-renderer rend) ;; Connect the renderer to the display library

(defun rndflake (i)
  (list (- (mod (rand) 640) 320) ;; x
        (- (mod (rand) 400) 200) ;; y
        (/ (to-float (mod (rand) 2000)) 1000.0) ;; z
        (mod (rand) 360))) ;; rotation

          
(define snowflakes (map rndflake (range 100)))

(defun renderflake (tgt img pz flake) {
       (var (x y z . a) flake)
       (if (< pz z) {
         (var nz (- z pz))
         (setq nz (if (= 0.0 nz) 0.1 nz))
         (var nx (+ (/ (to-float x) nz) 160))
         (var ny (+ (/ (to-float y) nz) 100))
         (var s (- 1.0 (/ nz 2.0)))
         (setq s (if (<= s 0.1) 0.1 s))
         ;(print nz " " s " " nx " " ny " " a " " pz " " z)
         (img-blit tgt img nx ny 0  `(scale ,s) `(rotate 64 64 ,(to-i a)))
         })
       })



(defun renderflakes (tgt img pz)
  (loopforeach flake snowflakes {
               (renderflake tgt img pz flake)
               }))


(define img (img-buffer 'indexed2 128 128))
(define tgt (img-buffer 'indexed2 320 200))
(define disp (img-buffer 'indexed2 320 200))



(img-clear tgt)
(start-figure tgt img '(64 . 64) 0 30)

(define color (img-color 'gradient_x 0xFF0000 0x0000FF 320 0 'repeat))

(loopwhile t {
           (loopfor i 0 (< i 50) (+ i 1) {
                    (img-clear tgt)     
                    (img-clear disp)
                    (renderflakes tgt img (* 2.0 (/ (to-float i) 100.0)))
                    (img-blit disp tgt -160 -100 -1 '(scale 2.0) `(rotate 160 100 ,i))
                    (disp-render disp 0 0 (list (img-color 'regular 0x000000)
                                                color))
                                        ;(sleep 0.1)
                    })
           (loopfor i 50 (> i -100) (- i 1) {
                    (img-clear tgt)
                    (img-clear disp)
                    (renderflakes tgt img (* 2.0 (/ (to-float i) 100.0)))
                    (img-blit disp tgt -160 -100 -1 '(scale 2.0) `(rotate 160 100 ,i))
                    (disp-render disp 0 0 (list (img-color 'regular 0x000000)
                                                color))
                                        ;(sleep 0.1)
                    })
           (loopfor i -100 (< i 0) (+ i 1) {
                    (img-clear tgt)
                    (img-clear disp)
                    (renderflakes tgt img (* 2.0 (/ (to-float i) 100.0)))
                    (img-blit disp tgt -160 -100 -1 '(scale 2.0) `(rotate 160 100 ,i))
                    (disp-render disp 0 0  (list (img-color 'regular 0x000000)
                                                 color))
                                        ;(sleep 0.1)
                    })
            (loopfor i 0 (< i 50) (+ i 1) {
                    (img-clear tgt)
                    (img-clear disp)
                    (renderflakes tgt img (* 2.0 (/ (to-float i) 100.0)))
                    (img-blit disp tgt -160 -100 -1 '(scale 2.0) `(rotate 160 100 ,(- (* 2.0 i))))
                    (disp-render disp 0 0 (list (img-color 'regular 0x000000)
                                                color))
                                        ;(sleep 0.1)
                    })
            (loopfor i 50 (> i 0) (- i 1) {
                    (img-clear tgt)
                    (img-clear disp)
                    (renderflakes tgt img (* 2.0 (/ (to-float i) 100.0)))
                    (img-blit disp tgt -160 -100 -1 '(scale 2.0) `(rotate 160 100 ,(- (* 2.0 i))))
                    (disp-render disp 0 0 (list (img-color 'regular 0x000000)
                                                color))
                                        ;(sleep 0.1)
                    })
           })
