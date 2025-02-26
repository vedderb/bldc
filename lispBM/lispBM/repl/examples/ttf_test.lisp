
(sdl-init)

(define win (sdl-create-window "TTF Font" 400 200))
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

(define img  (img-buffer 'rgb332 32 32))
(define disp (img-buffer 'rgb332 400 200))

;;(define font-file (fopen "HelveticaNeue-Bold.ttf" "r"))
(define font-file (fopen "Ubuntu-Regular.ttf" "r"))
(define font (load-file font-file))
(define ttf (ttf-font 32 32 font))
(setq ttf (ttf-prepare ttf 'rgb332 "TTF Font"))
(ttf-text disp 10 80 ttf "TTF Font")

(disp-render disp 0 0 (list 0x000000
                            0xFFFFFF))


