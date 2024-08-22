

(sdl-init)

(define w 500)
(define h 500)

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



(defun draw-figure (id rend p ang s)
  (if (<= s 2)
      (send id 'worker)
    (let ((p1 (move p ang s))
          (p2 (move p (+ ang 60) s))
          (p3 (move p (+ ang 120) s))
          (p4 (move p (+ ang 180) s))
          (p5 (move p (+ ang 240) s))
          (p6 (move p (+ ang 300) s)))
      (progn
        (line rend p p1)
        (line rend p p2)
        (line rend p p3)
        (line rend p p4)
        (line rend p p5)
        (line rend p p6)
        (sdl-present rend)
        (send id (list p1 (+ ang 10) (/ s 2.3)))
        (send id (list p2 (+ ang 70) (/ s 2.3)))
        (send id (list p3 (+ ang 130) (/ s 2.3)))
        (send id (list p4 (+ ang 190) (/ s 2.3)))
        (send id (list p5 (+ ang 250) (/ s 2.3)))
        (send id (list p6 (+ ang 310) (/ s 2.3)))
        (send id 'worker)))))

(defun schedule (rend workers work)
  (if (or (eq workers nil)
          (eq work nil))
      (cons workers work)
    (let ((workers-next (cdr workers))
          (w (car work))
          (work-next (cdr work))
          (p (car w))
          (ang (car (cdr w)))
          (s (car (cdr (cdr w)))))
      (progn
        (spawn draw-figure (self) rend p ang s)
        (schedule rend workers-next work-next)))))

(defun manager-start (rend workers work)
  (progn
    (set-mailbox-size 26000)
    (manager rend workers work)))

(defun manager (rend workers work)
  (let ((next (schedule rend workers work))
        (workers-next (car next))
        (work-next    (cdr next)))
      (recv ( worker (manager rend (cons 'worker workers-next) work-next))
            ( (? x)  (manager rend workers-next (cons x work-next))))
      ))

(define workers (list 'worker 'worker 'worker 'worker 'worker 'worker))
(define work (list
              '((250 . 250) 0 100)
              '((250 . 250) 60 100)
              '((250 . 250) 120 100)
              '((250 . 250) 180 100)
              '((250 . 250) 240 100)
              '((250 . 250) 300 100)))
              

(defun main ()
    (progn
      (define win (sdl-create-window "LISP-GFX" 500 500))
      (define rend (sdl-create-soft-renderer win)) 
      (spawn 100 event-loop win)
      (sdl-renderer-set-color rend 0 0 0)
      (sdl-clear rend)
      (sdl-renderer-set-color rend 255 255 255)

      (spawn manager-start rend workers work)

      (sdl-present rend)
      'done
      ))

(defun clean ()
  (gc))
