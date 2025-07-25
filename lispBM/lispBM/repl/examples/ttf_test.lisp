
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
(define ttf (ttf-prepare font 32 'indexed4 "TTF Font"))
(define aa-green '(0 17408 39168 65280))
(ttf-text disp 10 80 aa-green ttf "TTF Font")

(disp-render disp 0 0 (list ))


