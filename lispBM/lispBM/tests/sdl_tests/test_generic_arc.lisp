(sdl-init)

(define win (sdl-create-window "Generic Arc Test" 400 200))
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

(define img400x200 (img-buffer 'indexed16 400 200))

;; Clear the image buffer with black background
(img-clear img400x200 0)

;; Test 1: Basic dotted arc using img-arc
;; This should trigger generic_arc since dotted pattern with dot1 > 0
(define test1 (img-arc img400x200 80 60 30 0 180 1 '(dotted 5 3)))

;; Test 2: Full dotted circle using img-circle
;; This calls arc internally with 0-360 degrees and dotted pattern
(define test2 (img-circle img400x200 200 60 25 2 '(dotted 4 2)))

;; Test 3: Dotted circle sector (pie slice)
;; Tests generic_arc via img-circle-sector
(define test3 (img-circle-sector img400x200 320 60 25 45 135 3 '(dotted 6 2)))

;; Test 4: Dotted circle segment (chord)
;; Tests generic_arc via img-circle-segment
(define test4 (img-circle-segment img400x200 80 140 30 30 150 4 '(dotted 3 4)))

;; Test 5: Various dotted patterns - small dots, close spacing
(define test5 (img-arc img400x200 200 140 20 270 450 5 '(dotted 2 1)))

;; Test 6: Large dots, wide spacing
(define test6 (img-arc img400x200 320 140 25 -45 225 6 '(dotted 8 5)))

;; Test 7: Single pixel dots (dot1=1)
(define test7 (img-arc img400x200 350 100 15 0 360 7 '(dotted 1 2)))

;; Test 8: Different angles that might trigger different code paths
(define test8 (img-arc img400x200 50 100 15 90 270 8 '(dotted 3 2)))

;; Test 9: Arc spanning multiple quadrants
(define test9 (img-arc img400x200 150 100 20 45 315 9 '(dotted 4 3)))

;; Test 10: Very small radius with dotted pattern
(define test10 (img-arc img400x200 30 30 8 0 360 10 '(dotted 2 1)))

;; Test edge cases and error conditions
;; Test 11: Zero dot1 (should NOT call generic_arc)
(define test11 (img-arc img400x200 370 30 15 0 90 1 '(dotted 0 5)))

;; Test 12: Negative dot1 (should NOT call generic_arc)  
(define test12 (img-arc img400x200 370 50 15 0 90 2 '(dotted -5 5)))

;; Test 13: Test filled arc with dotted (should NOT call generic_arc)
(define test13 (img-circle img400x200 370 170 12 3 '(filled) '(dotted 5 3)))

;; Test 14: Test invalid parameters
(define test14 (trap (img-arc "invalid" 100 100 20 0 90 1 '(dotted 5 3))))

(define test15 (img-arc img400x200 100 100 -5 0 90 1 '(dotted 5 3)))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0x111111 0x222222 0x333333 0x444444 0x555555 0x666666 0x777777 
                               0x888888 0x999999 0xAAAAAA 0xBBBBBB 0xCCCCCC 0xDDDDDD 0xEEEEEE 0xFFFFFF))

;; Check test results
;; Tests 1-13 should succeed (return t)
;; Tests 14-15 should return error conditions

(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10 
         test11 test12 test13
         (eq test14 '(exit-error type_error))
         test15)
    (print "SUCCESS")
    (print "FAILURE"))
