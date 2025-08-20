(sdl-init)

(define win (sdl-create-window "Circle Segment Slice Edge Case Test" 600 400))
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

;; Tests targeting line 1380 in handle_arc_slice
;; Conditions needed:
;; - segment=true, filled=false (img-circle-segment without 'filled)
;; - angle0 < angle1 < 180 degrees
;; - in_both_caps && slice_overlaps0 && slice_overlaps1
;; - Thick enough to create slice intersections

;; Test 1: 45-135 degrees, thickness 6, radius 50
;; This should create geometry where slices intersect both caps
(define test1 (img-circle-segment img600x400 100 100 50 45 135 1 '(thickness 6)))

;; Test 2: 30-150 degrees, thickness 8, radius 60
;; Wider angle range, thicker line
(define test2 (img-circle-segment img600x400 250 100 60 30 150 2 '(thickness 8)))

;; Test 3: 60-120 degrees, thickness 10, radius 45
;; Centered around 90 degrees, very thick
(define test3 (img-circle-segment img600x400 400 100 45 60 120 3 '(thickness 10)))

;; Test 4: 15-165 degrees, thickness 5, radius 55
;; Very wide angle, just under 180 degrees
(define test4 (img-circle-segment img600x400 500 100 55 15 165 4 '(thickness 5)))

;; Test 5: 45-90 degrees, thickness 12, radius 40
;; Quarter circle with thick line
(define test5 (img-circle-segment img600x400 100 200 40 45 90 5 '(thickness 12)))

;; Test 6: 30-120 degrees, thickness 7, radius 65
;; Larger radius with moderate thickness
(define test6 (img-circle-segment img600x400 250 200 65 30 120 6 '(thickness 7)))

;; Test 7: 75-105 degrees, thickness 15, radius 35
;; Narrow angle around 90 degrees, very thick
(define test7 (img-circle-segment img600x400 400 200 35 75 105 7 '(thickness 15)))

;; Test 8: 10-170 degrees, thickness 4, radius 70
;; Almost semicircle, thinner line, large radius
(define test8 (img-circle-segment img600x400 500 200 70 10 170 8 '(thickness 4)))

;; Test specific radii that might trigger edge cases (61, 62, 63)
;; Test 9: Radius 61, 45-135 degrees, thickness 8
(define test9 (img-circle-segment img600x400 100 300 61 45 135 9 '(thickness 8)))

;; Test 10: Radius 62, 30-150 degrees, thickness 6
(define test10 (img-circle-segment img600x400 250 300 62 30 150 10 '(thickness 6)))

;; Test 11: Radius 63, 60-120 degrees, thickness 10
(define test11 (img-circle-segment img600x400 400 300 63 60 120 11 '(thickness 10)))

;; Test edge angles just under 180 degrees
;; Test 12: 0-179 degrees, thickness 5, radius 50
(define test12 (img-circle-segment img600x400 500 300 50 0 179 12 '(thickness 5)))

;; Test 13: 1-179 degrees, thickness 7, radius 45
(define test13 (img-circle-segment img600x400 50 50 45 1 179 13 '(thickness 7)))

;; Test 14: 89-179 degrees, thickness 9, radius 55
(define test14 (img-circle-segment img600x400 150 50 55 89 179 14 '(thickness 9)))

;; Test angles that should maximize slice intersection probability
;; Test 15: 20-160 degrees, thickness 6, radius 48
(define test15 (img-circle-segment img600x400 250 50 48 20 160 15 '(thickness 6)))

;; Test 16: 40-140 degrees, thickness 8, radius 52
(define test16 (img-circle-segment img600x400 350 50 52 40 140 1 '(thickness 8)))

;; Test 17: 25-155 degrees, thickness 7, radius 58
(define test17 (img-circle-segment img600x400 450 50 58 25 155 2 '(thickness 7)))

;; Test with rounded ends (which might affect cap line calculations)
;; Test 18: 45-135 degrees, thickness 8, radius 50, rounded
(define test18 (img-circle-segment img600x400 550 50 50 45 135 3 '(rounded) '(thickness 8)))

;; Test 19: 30-150 degrees, thickness 10, radius 45, rounded
(define test19 (img-circle-segment img600x400 50 150 45 30 150 4 '(rounded) '(thickness 10)))

;; Test smaller angles that still might trigger the condition
;; Test 20: 70-110 degrees, thickness 12, radius 40
(define test20 (img-circle-segment img600x400 150 150 40 70 110 5 '(thickness 12)))

;; Test 21: 50-130 degrees, thickness 9, radius 55
(define test21 (img-circle-segment img600x400 250 150 55 50 130 6 '(thickness 9)))

;; Test 22: 35-145 degrees, thickness 11, radius 42
(define test22 (img-circle-segment img600x400 350 150 42 35 145 7 '(thickness 11)))

;; Test with very specific angles that might create the geometric conditions
;; Test 23: 22.5-157.5 degrees (135 degree span), thickness 6, radius 50
(define test23 (img-circle-segment img600x400 450 150 50 22.5 157.5 8 '(thickness 6)))

;; Test 24: 67.5-112.5 degrees (45 degree span), thickness 14, radius 38
(define test24 (img-circle-segment img600x400 550 150 38 67.5 112.5 9 '(thickness 14)))

;; Test 25: 11.25-168.75 degrees, thickness 5, radius 60
(define test25 (img-circle-segment img600x400 50 250 60 11.25 168.75 10 '(thickness 5)))

;; Test very thick segments that should definitely intersect both caps
;; Test 26: 45-135 degrees, thickness 20, radius 50
(define test26 (img-circle-segment img600x400 150 250 50 45 135 11 '(thickness 20)))

;; Test 27: 30-150 degrees, thickness 18, radius 55
(define test27 (img-circle-segment img600x400 250 250 55 30 150 12 '(thickness 18)))

;; Test 28: 60-120 degrees, thickness 16, radius 45
(define test28 (img-circle-segment img600x400 350 250 45 60 120 13 '(thickness 16)))

;; Test error conditions (should not crash)
;; Test 29: Invalid image buffer
(define test29 (trap (img-circle-segment "invalid" 100 100 50 45 135 1 '(thickness 6))))

;; Test 30: Invalid color
(define test30 (trap (img-circle-segment img600x400 100 100 50 45 135 "invalid" '(thickness 6))))

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
         test21 test22 test23 test24 test25 test26 test27 test28
         (eq test29 '(exit-error type_error))
         (eq test30 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))