(sdl-init)

(define win (sdl-create-window "Display library - indexed16 test" 400 200))
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

;; Test indexed16 format (4 bits per pixel, 16 colors)
(define img400x200 (img-buffer 'indexed16 400 200))

;; Test basic drawing operations with indexed16
(define r1 (img-rectangle img400x200 10 10 50 30 1))
(define r2 (img-circle img400x200 100 50 20 2))
(define r3 (img-triangle img400x200 150 20 180 60 120 60 3))
(define r4 (img-line img400x200 200 10 350 40 4))
(define r5 (img-arc img400x200 300 100 30 0 180 5))

;; Test with color values beyond indexed2/indexed4 range (0-15 valid for indexed16)
(define r6 (img-rectangle img400x200 50 100 40 30 8))
(define r7 (img-circle img400x200 150 130 15 12))
(define r8 (img-triangle img400x200 200 100 230 140 170 140 15))

;; Test with invalid color values (should clamp/wrap)
(define r9 (img-rectangle img400x200 300 120 50 30 20)) ; Color > 15
(define r10 (img-line img400x200 10 170 100 170 255))   ; Color >> 15

;; Test buffer dimensions and format
(define dims (img-dims img400x200))
(define is_buffer (img-buffer? img400x200))

;; Display the result with 16-color palette
(disp-render img400x200 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 
                               0xFFFF00 0xFF00FF 0x00FFFF 0xFFFFFF
                               0x800000 0x008000 0x000080 0x808000
                               0x800080 0x008080 0x808080 0xC0C0C0))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 is_buffer
         (eq dims '(400 200)))
    (print "SUCCESS")
    (print "FAILURE"))
