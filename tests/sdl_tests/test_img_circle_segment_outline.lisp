(sdl-init)

(define win (sdl-create-window "Circle Segment Outline Test" 600 400))
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

(define img600x400 (img-buffer 'indexed16 600 400))

;; Clear the image buffer with black background
(img-clear img600x400 0)

;; Test img-circle-segment WITHOUT filled attribute
;; This will call handle_arc_slice with segment=true and filled=false

;; Test 1: Basic circle segment (chord outline) - 90 degrees
(define test1 (img-circle-segment img600x400 100 100 50 0 90 1))

;; Test 2: Circle segment - 180 degrees (semicircle chord)
(define test2 (img-circle-segment img600x400 250 100 50 0 180 2))

;; Test 3: Circle segment - 270 degrees (3/4 circle chord)
(define test3 (img-circle-segment img600x400 400 100 50 0 270 3))

;; Test 4: Circle segment - small angle (30 degrees)
(define test4 (img-circle-segment img600x400 500 100 40 0 30 4))

;; Test 5: Circle segment with different starting angle
(define test5 (img-circle-segment img600x400 100 200 45 45 135 5))

;; Test 6: Circle segment - almost full circle (350 degrees)
(define test6 (img-circle-segment img600x400 250 200 45 10 360 6))

;; Test critical radii that might trigger edge cases in handle_arc_slice
;; Test 7: Radius 61 - 120 degrees
(define test7 (img-circle-segment img600x400 100 300 61 0 120 7))

;; Test 8: Radius 62 - 150 degrees
(define test8 (img-circle-segment img600x400 230 300 62 0 150 8))

;; Test 9: Radius 63 - 240 degrees
(define test9 (img-circle-segment img600x400 370 300 63 0 240 9))

;; Test critical angles that might reveal edge cases
;; Test 10: 179 degrees (just under semicircle)
(define test10 (img-circle-segment img600x400 500 300 35 0 179 10))

;; Test 11: 181 degrees (just over semicircle)  
(define test11 (img-circle-segment img600x400 50 50 30 0 181 11))

;; Test 12: 359 degrees (almost full circle)
(define test12 (img-circle-segment img600x400 150 50 25 0 359 12))

;; Test with thickness to exercise more of handle_arc_slice
;; Test 13: Thick circle segment - thickness 3
(define test13 (img-circle-segment img600x400 250 50 30 45 180 13 '(thickness 3)))

;; Test 14: Very thick circle segment - thickness 8
(define test14 (img-circle-segment img600x400 350 50 25 0 90 14 '(thickness 8)))

;; Test 15: Maximum practical thickness - thickness 16
(define test15 (img-circle-segment img600x400 450 50 20 30 150 15 '(thickness 16)))

;; Test with rounded ends
;; Test 16: Rounded circle segment - thickness 4
(define test16 (img-circle-segment img600x400 550 50 25 0 120 1 '(rounded) '(thickness 4)))

;; Test very small and very large angles with different radii
;; Test 17: Very small angle (0.5 degrees) - radius 40
(define test17 (img-circle-segment img600x400 50 150 40 0 0.5 2))

;; Test 18: Very small angle (1 degree) - radius 50
(define test18 (img-circle-segment img600x400 150 150 50 0 1 3))

;; Test 19: Large angle (300 degrees) - radius 30
(define test19 (img-circle-segment img600x400 350 150 30 0 300 4))

;; Test 20: Crossing 360 degrees boundary (370 degrees)
(define test20 (img-circle-segment img600x400 450 150 25 0 370 5))

;; Test negative angles
;; Test 21: Negative start angle
(define test21 (img-circle-segment img600x400 550 150 30 -45 45 6))

;; Test 22: Both angles negative
(define test22 (img-circle-segment img600x400 50 250 25 -90 -30 7))

;; Test edge cases with specific angle combinations
;; Test 23: Start > End angle (should wrap around)
(define test23 (img-circle-segment img600x400 150 250 35 270 45 8))

;; Test 24: Same start and end angle (degenerate case)
(define test24 (img-circle-segment img600x400 250 250 30 90 90 9))

;; Test 25: Zero radius (edge case)
(define test25 (img-circle-segment img600x400 350 250 0 0 90 10))

;; Test 26: Very small radius (1 pixel)
(define test26 (img-circle-segment img600x400 450 250 1 0 180 11))

;; Test 27: Large radius approaching image boundary
(define test27 (img-circle-segment img600x400 550 250 45 0 45 12))

;; Test error conditions
;; Test 28: Invalid image buffer
(define test28 (trap (img-circle-segment "invalid" 100 100 50 0 90 1)))

;; Test 29: Invalid color
(define test29 (trap (img-circle-segment img600x400 100 100 50 0 90 "invalid")))

;; Test 30: Negative radius (should work - takes absolute value)
(define test30 (img-circle-segment img600x400 500 350 -30 0 120 13))

;; Display the result with a 16-color palette
(disp-render img600x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 
                               0xFFFF00 0xFF00FF 0x00FFFF 0xFFFFFF
                               0x800000 0x008000 0x000080 0x808000
                               0x800080 0x008080 0x808080 0xC0C0C0))

;; Check test results
;; Most drawing operations should succeed (return t)
;; Error conditions should return appropriate error types
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
         test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
         test21 test22 test23 test24 test25 test26 test27 test30
         (eq test28 '(exit-error type_error))
         (eq test29 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))