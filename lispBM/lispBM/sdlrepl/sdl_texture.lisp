


(sdl-init)

(define w 500)
(define h 500)

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))


(defun draw-loop (rend tex x y dx dy)
    (let ((cx (and (< dx 0) (<= x 0)))
	  (cy (and (< dy 0) (<= y 0)))
	  (cx-high (and (> dx 0) (>= x 420)))
	  (cy-high (and (> dy 0) (>= y 420)))
	  (dx-new (if (or cx cx-high) (* -1 dx) dx))
	  (dy-new (if (or cy cy-high) (* -1 dy) dy)))
      (progn
	(sdl-clear rend)
	(sdl-blit rend tex x y 80 80)
	(sdl-present rend)
	(yield 8000)
	(draw-loop rend tex (+ x dx-new) (+ y dy-new) dx-new dy-new)
	)
      )
    )

(defun main ()
  (let ((win (sdl-create-window "LISP-GFX" 500 500))
        (rend (sdl-create-soft-renderer win))
	(tex (sdl-load-texture rend "tex1.png")))
    (progn
      (spawn 100 event-loop win)
      (sdl-renderer-set-color rend 0 0 0)
      (sdl-clear rend)

      (draw-loop rend tex 17 250 -1 3)
      )))

(defun clean ()
  (gc))
