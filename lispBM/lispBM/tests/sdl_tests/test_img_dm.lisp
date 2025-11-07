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

;; Create a DM region (defragmentable memory)
(define dm (dm-create (* 400 200 )))

(define r3 (eq (type-of dm) type-dm))

;; create 2 images in the DM
(define img1 (img-buffer dm 'indexed2 50 50))
(define img2 (img-buffer 'indexed2 100 100))

;; Draw something 
(define r1 (img-rectangle img1 10 10 30 30 1))
(define r2 (img-circle img2 50 50 25 1))

;; Display the result
(disp-render img1 0 0 '(0x000000 0xFFFFFF))
(disp-render img2 50 50 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3)
    (print "SUCCESS")
    (print "FAILURE"))
