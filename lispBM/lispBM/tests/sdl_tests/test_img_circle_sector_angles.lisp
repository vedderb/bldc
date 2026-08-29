(sdl-init)

(define win (sdl-create-window "Display library - circle sector angles test" 500 100))
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

(define img500x100 (img-buffer 'indexed2 500 100))

;; Sweep angle, smallest to largest, no other attributes
(define r1 (img-circle-sector img500x100 60 50 40 0 45 1))    ; 45 degrees
(define r2 (img-circle-sector img500x100 160 50 40 0 90 1))   ; 90 degrees
(define r3 (img-circle-sector img500x100 260 50 40 0 180 1))  ; 180 degrees
(define r4 (img-circle-sector img500x100 360 50 40 0 270 1))  ; 270 degrees
(define r5 (img-circle-sector img500x100 460 50 40 0 360 1))  ; 360 degrees

;; Test buffer properties
(define dims (img-dims img500x100))
(define is_buffer (img-buffer? img500x100))

;; Display the result
(disp-render img500x100 0 0 '(0x000000 0xFFFFFF))
(save-img img500x100 "sdl_tests/png_out/test_img_circle_sector_angles.png" '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5
         is_buffer (eq dims '(500 100)))
    (print "SUCCESS")
    (print "FAILURE"))
