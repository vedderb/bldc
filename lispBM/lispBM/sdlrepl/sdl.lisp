


(sdl-init)

(define w 500)
(define h 500)

(defun degtorad (d)
  (/ (* d 3.141) 180.0))

(defun rotate (p angle)
  `(,(- (* (car p) (cos (degtorad angle)))
        (* (cdr p) (sin (degtorad angle))))
    .
    ,(+ (* (car p) (sin (degtorad angle)))
        (* (cdr p) (cos (degtorad angle))))))


(defun trans (p v)
  `(,(+ (car p) (car v)) . ,(+ (cdr p) (cdr v))))

(defun move (p ang s)
  (let ((v (rotate `( 0 . ,(- s)) ang)))
    (trans p v)))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(defun line (rend p1 p2)
  (sdl-draw-line rend (car p1) (cdr p1) (car p2) (cdr p2)))


(defun draw-figure (rend p ang s)
  (if (<= s 1)
      ()
      (let ((p1 (move p ang s)))
        (progn
          (line rend p p1)
          (draw-figure rend p1 (+ ang 2) (- s 1))
          (draw-figure rend p1 (- ang 27) (/ s 2))
          (draw-figure rend p1 (+ ang 27) (/ s 2))))))

(defun main ()
  (let ((win (sdl-create-window "LISP-GFX" 500 500))
        (rend (sdl-create-soft-renderer win)))
    (progn
      (spawn 100 event-loop win)
      (sdl-renderer-set-color rend 0 0 0)
      (sdl-clear rend)
      (sdl-renderer-set-color rend 255 255 255)

      (draw-figure rend '(250 . 400) 0 25)

      (sdl-present rend)
      (custom-destruct rend)
      'done
      )))

(defun clean ()
  (gc))
