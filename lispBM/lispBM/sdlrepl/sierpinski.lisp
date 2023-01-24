(sdl-init)

(define w 500)
(define h 500)

(define corners '((10 . 490) (490 . 490) (250 . 10)))


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

(defun point (rend p)
  (sdl-draw-point rend (car p) (cdr p)))

;; (defun draw-figure (rend p ang s)
;;   (if (<= s 1)
;;       ()
;;       (let ((p1 (move p ang s)))
;;         (progn
;;           (line rend p p1)
;;           (draw-figure rend p1 (+ ang 2) (- s 1))
;;           (draw-figure rend p1 (- ang 27) (/ s 2))
;;           (draw-figure rend p1 (+ ang 27) (/ s 2))))))


(defun mid-point (p1 p2)
  (progn
    ;(print "p1: " p1)
    ;(print "p2: " p2)
    (let ((x (/ (+ (car p1) (car p2)) 2))
          (y (/ (+ (cdr p1) (cdr p2)) 2)))
      (cons x y))))

(defun sierpinsky (n rend corners p)
  (if (= n 0) ()
    (let ((i (mod (random) 3))
          (target (ix corners i))
          (mid    (mid-point p target)))
      (progn
        ;(print "p " p)
        ;(print "target " target)
        ;(print "mid "mid)
        ;(print "i " i)
        (point rend mid)
        (sdl-present rend)
        (sierpinsky (- n 1) rend corners mid))
      )))
          
      
    

(defun draw-figure (rend)
  (progn
    (seed 3)
    (map (point rend) corners)
    (sierpinsky 25000 rend corners (car corners))))






(defun main ()
  (let ((win (sdl-create-window "LISP-GFX" 500 500))
        (rend (sdl-create-soft-renderer win)))
    (progn
      (spawn 100 event-loop win)
      (sdl-renderer-set-color rend 0 0 0)
      (sdl-clear rend)
      (sdl-renderer-set-color rend 255 255 255)

      (draw-figure rend)

      (sdl-present rend)
      
      ;;(custom-destruct rend)
      'done
      )))

(defun clean ()
  (gc))
