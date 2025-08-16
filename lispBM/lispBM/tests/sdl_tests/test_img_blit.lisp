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

;; Create source and destination images
(define src_img (img-buffer 'indexed2 50 50))
(define dst_img (img-buffer 'indexed2 400 200))

;; Draw something in source image
(img-rectangle src_img 10 10 30 30 1)
(img-circle src_img 25 25 10 1)

;; Test img-blit function
;; Basic blit with color depth -1 (use all)
(define r1 (img-blit dst_img src_img 50 50 -1))

;; Blit to different position
(define r2 (img-blit dst_img src_img 150 100 -1))

;; Blit partially outside bounds
(define r3 (img-blit dst_img src_img -10 -10 -1))
(define r4 (img-blit dst_img src_img 380 180 -1))

;; Test with same image as source and destination
(define r5 (img-blit dst_img dst_img 100 20 -1))

;; Test with invalid source image - returns t (doesn't validate source properly)
(define r6 (trap (img-blit dst_img "not-an-image" 0 0 -1)))
(define r7 (trap (img-blit dst_img nil 0 0 -1)))

;; Test with invalid destination image - does validate destination
(define r8 (trap (img-blit "not-an-image" src_img 0 0 -1)))
(define r9 (trap (img-blit nil src_img 0 0 -1)))

;; Display the result
(disp-render dst_img 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5
         (eq r6 '(exit-error type_error))
         (eq r7 '(exit-error type_error))
         (eq r8 '(exit-error type_error)) (eq r9 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
