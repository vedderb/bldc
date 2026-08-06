(sdl-init)

(define win (sdl-create-window "Display library - rounded rectangle thickness test" 660 150))
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

(define img660x150 (img-buffer 'indexed4 660 150))

;; Rounded rectangles, thickness 0 through 6, same radius and size.
(define r0 (img-rectangle img660x150  20 30 70 70 1 '(rounded 15) '(thickness 0)))
(define r1 (img-rectangle img660x150 110 30 70 70 2 '(rounded 15) '(thickness 1)))
(define r2 (img-rectangle img660x150 200 30 70 70 3 '(rounded 15) '(thickness 2)))
(define r3 (img-rectangle img660x150 290 30 70 70 1 '(rounded 15) '(thickness 3)))
(define r4 (img-rectangle img660x150 380 30 70 70 2 '(rounded 15) '(thickness 4)))
(define r5 (img-rectangle img660x150 470 30 70 70 3 '(rounded 15) '(thickness 5)))
(define r6 (img-rectangle img660x150 560 30 70 70 1 '(rounded 15) '(thickness 6)))

;; Test buffer properties
(define dims (img-dims img660x150))
(define is_buffer (img-buffer? img660x150))

;; Display the result with 4-color palette
(disp-render img660x150 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))
(save-img img660x150 "sdl_tests/png_out/test_img_rounded_rectangle_thickness.png" '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r0 r1 r2 r3 r4 r5 r6
         is_buffer (eq dims '(660 150)))
    (print "SUCCESS")
    (print "FAILURE"))
