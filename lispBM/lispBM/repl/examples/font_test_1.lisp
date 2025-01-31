(sdl-init)

(define win (sdl-create-window "AA Font" 320 200))
(define rend (sdl-create-soft-renderer win))

(define font-file (fopen "font_22_24_aa.bin" "r"))
(define font (load-file font-file))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

(sdl-set-active-renderer rend) ;; Connect the renderer to the display library

(defun renderflakes (tgt img pz)
  (loopforeach flake snowflakes {
               (renderflake tgt img pz flake)
               }))

(define disp (img-buffer 'indexed4 320 200))

(print (type-of font))

(img-text disp 10 10 '(0 1 2 3) font "non-const font")

(move-to-flash font)

(print (type-of font))
(img-text disp 10 35 '(0 1 2 3) font "const font")

(disp-render disp 0 0 (list 0x000000
                            0x555555
                            0x999999
                            0xFFFFFF))

