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

;; Test img-color-set function
;; Create a color and modify it
(define color1 (img-color 'regular 0xFF0000))  ;; Red

;; Test setting properties on regular color
(define r1 (img-color-set color1 'color-0 0x00FF00))  ;; Change to green - valid for regular
(define r2 (trap (img-color-set color1 'color-1 0x0000FF)))  ;; Invalid for regular color
(define r3 (trap (img-color-set color1 'width 150)))          ;; Invalid for regular color  
(define r4 (trap (img-color-set color1 'offset 25)))          ;; Invalid for regular color

;; Test with gradient color
(define color2 (img-color 'gradient_x 0xFF0000 0x0000FF 100 0))
(define r5 (img-color-set color2 'color-0 0x00FF00))
(define r6 (img-color-set color2 'width 200))
(define r7 (img-color-set color2 'offset 50))

;; Test setting repeat type
(define r8 (img-color-set color2 'repeat-type 'mirrored))

;; Test with invalid color object
(define r9 (trap (img-color-set "not-a-color" 'color-0 0xFF0000)))
(define r10 (trap (img-color-set nil 'color-0 0xFF0000)))

;; Test with invalid property name
(define r11 (trap (img-color-set color1 'invalid-property 0xFF0000)))

;; Test with invalid property value
(define r12 (trap (img-color-set color1 'color-0 "not-a-number")))

;; Verify that the functions return true for valid operations
(if (and r1 r5 r6 r7 r8
         (eq r2 '(exit-error type_error)) (eq r3 '(exit-error type_error)) (eq r4 '(exit-error type_error))
         (eq r9 '(exit-error type_error)) (eq r10 '(exit-error type_error))
         (eq r11 '(exit-error type_error)) (eq r12 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))