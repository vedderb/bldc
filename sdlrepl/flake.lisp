

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


;; (defun draw-figure (rend p ang s)
;;   (if (<= s 1)
;;       ()
;;       (let ((p1 (move p ang s)))
;;         (progn
;;           (line rend p p1)
;;           (draw-figure rend p1 (+ ang 2) (- s 1))
;;           (draw-figure rend p1 (- ang 27) (/ s 2))
;;           (draw-figure rend p1 (+ ang 27) (/ s 2))))))



(defun draw-figure (rend p ang s)
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
        (line rend p p1)
        (line rend p p2)
        (line rend p p3)
        (line rend p p4)
        (line rend p p5)
        (line rend p p6)
        (sdl-present rend)
        (draw-figure rend p1 (+ ang 10) (/ s 2))
        (draw-figure rend p2 (+ ang 70) (/ s 2))
        (draw-figure rend p3 (+ ang 130) (/ s 2))
        (draw-figure rend p4 (+ ang 190) (/ s 2))
        (draw-figure rend p5 (+ ang 250) (/ s 2))
        (draw-figure rend p6 (+ ang 310) (/ s 2))))))


(defun start-figure (rend p ang s)
  (progn
    (spawn draw-figure rend p (+ ang ) s)
    (spawn draw-figure rend p (+ ang 60) s)
    (spawn draw-figure rend p (+ ang 120) s)
    (spawn draw-figure rend p (+ ang 180) s)
    (spawn draw-figure rend p (+ ang 240) s)
    (spawn draw-figure rend p (+ ang 300) s)))



(defun main ()
  (let ((win (sdl-create-window "LISP-GFX" 500 500))
        (rend (sdl-create-soft-renderer win)))
    (progn
      (spawn 100 event-loop win)
      (sdl-renderer-set-color rend 0 0 0)
      (sdl-clear rend)
      (sdl-renderer-set-color rend 255 255 255)

      (start-figure rend '(250 . 250) 0 100)

      (sdl-present rend)
      
      ;;(custom-destruct rend)
      'done
      )))

(defun clean ()
  (gc))
