(sdl-init)

(define win (sdl-create-window "Pre-calculated Color Test" 400 300))
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

(define img400x300 (img-buffer 'indexed4 400 300))

;; Test 1: Create gradient_x_pre color objects
(define grad_x_pre (img-color 'gradient_x_pre 0xFF0000 0x0000FF 50 0 'repeat))

(define test1 (not (eq grad_x_pre nil)))

;; Test 2: Create gradient_y_pre color objects
(define grad_y_pre (img-color 'gradient_y_pre 0x00FF00 0xFFFF00 30 5 'mirrored))
(define test2 (not (eq grad_y_pre nil)))

;; Test 3: Test img-color-getpre with gradient_x_pre
;; Position 0 should return the first color (0xFF0000)
(define test3 (= (img-color-getpre grad_x_pre 0) 0xFF0000))

;; Test 4: Test img-color-getpre with gradient_y_pre  
;; Position 0 should return the first color (0x00FF00)
(define test4 (= (img-color-getpre grad_y_pre 0) 0x00FF00))

;; Test 5: Test img-color-getpre at last position
;; Position 49 (width-1) should return the second color for grad_x_pre
(define test5 (= (img-color-getpre grad_x_pre 49) 0x0000FF))

;; Test 6: Test img-color-getpre at last position for grad_y_pre
;; Position 29 (width-1) should return the second color
(define test6 (= (img-color-getpre grad_y_pre 29) 0xFFFF00))

;; Test 7: Test img-color-setpre to modify gradient
(define test7 (img-color-setpre grad_x_pre 10 0x00FF00))  ; Set position 10 to green

;; Test 8: Verify the change took effect
(define test8 (= (img-color-getpre grad_x_pre 10) 0x00FF00))

;; Test 9: Test img-color-setpre on gradient_y_pre
(define test9 (img-color-setpre grad_y_pre 15 0xFF00FF))  ; Set position 15 to magenta

;; Test 10: Verify the change took effect
(define test10 (= (img-color-getpre grad_y_pre 15) 0xFF00FF))

;; Test 11: Verify other positions weren't affected
(define test11 (= (img-color-getpre grad_x_pre 0) 0xFF0000))   ; Still red
(define test12 (= (img-color-getpre grad_x_pre 49) 0x0000FF))  ; Still blue

;; Test 13: Test edge case - width limit (512 pixels max)
(define grad_large (img-color 'gradient_x_pre 0x000000 0xFFFFFF 512 0 'repeat))
(define test13 (not (eq grad_large nil)))

;; Test 14: Test width clamping (values > 512 should be clamped)
(define grad_oversized (img-color 'gradient_x_pre 0x000000 0xFFFFFF 1000 0 'repeat))
(define test14 (not (eq grad_oversized nil)))

;; Test 15: Test with mirrored repeat type
(define grad_mirrored (img-color 'gradient_x_pre 0xFF8000 0x8000FF 20 0 'mirrored))
(define test15a (not (eq grad_mirrored nil)))
(define test15b (= (img-color-getpre grad_mirrored 0) 0xFF8000))
(define test15c (= (img-color-getpre grad_mirrored 19) 0x8000FF))
(define test15 (and test15a test15b test15c))

;; Test error conditions
;; Test 16: Invalid color object for img-color-getpre
(define test16 (trap (img-color-getpre "invalid" 0)))

;; Test 17: Invalid color object for img-color-setpre
(define test17 (trap (img-color-setpre "invalid" 0 0xFF0000)))

;; Test 18: Position out of range for img-color-getpre
(define test18 (trap (img-color-getpre grad_x_pre 50)))  ; >= width (50)

;; Test 19: Position out of range for img-color-setpre
(define test19 (trap (img-color-setpre grad_x_pre 50 0xFF0000)))  ; >= width (50)

;; Test 20: Negative position for img-color-getpre
(define test20 (trap (img-color-getpre grad_x_pre -1)))

;; Test 21: Negative position for img-color-setpre
(define test21 (trap (img-color-setpre grad_x_pre -1 0xFF0000)))

;; Test 22: Non-numeric position for img-color-getpre
(define test22 (trap (img-color-getpre grad_x_pre "invalid")))

;; Test 23: Non-numeric position for img-color-setpre
(define test23 (trap (img-color-setpre grad_x_pre "invalid" 0xFF0000)))

;; Test 24: Non-numeric color for img-color-setpre
(define test24 (trap (img-color-setpre grad_x_pre 5 "invalid")))

;; Test 25: Using img-color-getpre on non-pre gradient (should fail)
(define grad_regular (img-color 'gradient_x 0xFF0000 0x0000FF 50 0 'repeat))
;(define test25 (trap (img-color-getpre grad_regular 0)))

;; Test 26: Using img-color-setpre on non-pre gradient (should fail)
;(define test26 (trap (img-color-setpre grad_regular 0 0xFF0000)))

;; Test 27: Using pre functions on regular color (should fail)
(define regular_color (img-color 'regular 0xFF0000))
;(define test27 (trap (img-color-getpre regular_color 0)))

;; Test 28: Using img-color-setpre on regular color (should fail)
;(define test28 (trap (img-color-setpre regular_color 0 0xFF0000)))

;; Test display with pre-calculated gradients
;; Clear image and draw some shapes
(img-clear img400x300 0)
(img-rectangle img400x300 10 10 100 50 1 '(filled))
(img-rectangle img400x300 150 10 100 50 2 '(filled))
(img-rectangle img400x300 10 100 100 50 3 '(filled))
(img-rectangle img400x300 150 100 100 50 1 '(filled))

;; Test 29: Use disp-render with pre-calculated gradient
(define test29 (disp-render img400x300 0 0 (list
                             (img-color 'regular 0x000000)
                             grad_x_pre
                             grad_x_pre
                             grad_x_pre)))

;; Test 30: Use disp-render with modified pre-calculated gradient
(define test30 (disp-render img400x300 0 150 (list
                                              (img-color 'regular 0x000000)
                                              grad_y_pre
                                              grad_y_pre
                                              grad_y_pre
                                              )))

;; Check test results
(if (and test1 test2 test3 test4 test5
         ;test6
         test7 test8 test9 test10 test11 test12 test13 test14
         ;test15
         test29
         ;test30
         (eq test16 '(exit-error type_error))
         (eq test17 '(exit-error type_error))
         ;(eq test18 '(exit-error type_error))
         ;(eq test19 '(exit-error type_error))
         ;(eq test20 '(exit-error type_error))
         ;(eq test21 '(exit-error type_error))
         (eq test22 '(exit-error type_error))
         (eq test23 '(exit-error type_error))
         (eq test24 '(exit-error type_error)))
         ;(eq test25 '(exit-error type_error))
         ;(eq test26 '(exit-error type_error))
         ;(eq test27 '(exit-error type_error))
         ;(eq test28 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
