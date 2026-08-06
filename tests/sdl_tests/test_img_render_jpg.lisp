(sdl-init)

(define win (sdl-create-window "Display library - img-render-jpg test" 400 300))
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

(define cat-pic-file (f-open "./sdl_tests/lispbm.jpeg" "r"))
(define cat-pic-jpg (load-file cat-pic-file))

(define img (img-buffer 'rgb888 400 300))

;; Decode into the buffer, no display needed for this to work
(define r1 (img-render-jpg img cat-pic-jpg 0 0))

;; Invalid destination buffer
(define r2 (trap (img-render-jpg "not-an-image" cat-pic-jpg 0 0)))

;; Valid array argument, but not JPEG data - decode fails, no error raised
(define r3 (img-render-jpg img "not-a-jpg" 0 0))
(define r4 (img-render-jpg img [1 2 3 4] 0 0))

;; Display the result
(disp-render img 0 0 nil)
(save-img img "sdl_tests/png_out/test_img_render_jpg.png" nil)

(if (and r1
         (eq r2 '(exit-error type_error))
         (eq r3 nil)
         (eq r4 nil))
    (print "SUCCESS")
    (print "FAILURE"))
