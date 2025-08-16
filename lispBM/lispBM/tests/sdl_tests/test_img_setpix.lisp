
(sdl-init)

(define win (sdl-create-window "Display library" 400 200))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

;; Connect the renderer to the display library
(sdl-set-active-renderer rend)

(define img400x200 (img-buffer 'indexed2 400 200))

;; testing setpix onto img400x200
(define r1 (img-setpix img400x200 10 10 1)) ;; color index 1
(define r2 (img-setpix img400x200 0 0 0)) ;; color index 1

(define r3 (img-setpix img400x200 1000 1000 1)) ;; outside of image
(define r4 (img-setpix img400x200 -100 -100 1)) ;; negative numbers

(define r5 (img-setpix img400x200 1000 1000 8)) ;; index out of range

(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5)
    (print "SUCCESS")
    (print "FAILURE"))
