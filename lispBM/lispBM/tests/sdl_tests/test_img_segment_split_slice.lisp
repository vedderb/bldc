(sdl-init)

(define win (sdl-create-window "Circle Segment Split Slice Test" 600 400))
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

;; Tests targeting lines 1415 and 1428 in handle_arc_slice
;; Both lines are in the "split slice" branch where angle0 >= angle1
;; This happens when arcs wrap around the 0°/360° boundary

;; CONDITIONS FOR LINE 1415:
;; - segment=true, filled=false
;; - angle0 >= angle1 (wrap-around)
;; - angle0 < 180 degrees
;; - in_both_caps && slice_overlaps0 && slice_overlaps1
;; - end_is_past0 == 1

;; Test 1: 170-30 degrees (wrap around), thickness 6, radius 50
;; Start < 180, wraps around 0/360 boundary
(define test1 (img-circle-segment img600x400 100 100 50 170 30 1 '(thickness 6)))

;; Test 2: 150-45 degrees (wrap around), thickness 8, radius 60
(define test2 (img-circle-segment img600x400 250 100 60 150 45 2 '(thickness 8)))

;; Test 3: 120-60 degrees (wrap around), thickness 10, radius 45
(define test3 (img-circle-segment img600x400 400 100 45 120 60 3 '(thickness 10)))

;; Test 4: 90-15 degrees (wrap around), thickness 5, radius 55
(define test4 (img-circle-segment img600x400 500 100 55 90 15 4 '(thickness 5)))

;; Test 5: 135-75 degrees (wrap around), thickness 12, radius 40
(define test5 (img-circle-segment img600x400 100 200 40 135 75 5 '(thickness 12)))

;; Test 6: 160-20 degrees (wrap around), thickness 7, radius 65
(define test6 (img-circle-segment img600x400 250 200 65 160 20 6 '(thickness 7)))

;; Test 7: 105-45 degrees (wrap around), thickness 15, radius 35
(define test7 (img-circle-segment img600x400 400 200 35 105 45 7 '(thickness 15)))

;; Test edge case: start angle just under 180 degrees
;; Test 8: 179-10 degrees (wrap around), thickness 8, radius 50
(define test8 (img-circle-segment img600x400 500 200 50 179 10 8 '(thickness 8)))

;; Test 9: 175-25 degrees (wrap around), thickness 6, radius 55
(define test9 (img-circle-segment img600x400 100 300 55 175 25 9 '(thickness 6)))

;; Test 10: 165-35 degrees (wrap around), thickness 9, radius 45
(define test10 (img-circle-segment img600x400 250 300 45 165 35 10 '(thickness 9)))

;; CONDITIONS FOR LINE 1428:
;; - segment=true, filled=false
;; - angle0 >= angle1 (wrap-around)
;; - angle0 >= 180 degrees
;; - in_both_caps && slice_overlaps0 && slice_overlaps1
;; - end_is_past1 == 1

;; Test 11: 270-90 degrees (wrap around), thickness 6, radius 50
;; Start >= 180, wraps around 0/360 boundary
(define test11 (img-circle-segment img600x400 400 300 50 270 90 11 '(thickness 6)))

;; Test 12: 315-45 degrees (wrap around), thickness 8, radius 60
(define test12 (img-circle-segment img600x400 500 300 60 315 45 12 '(thickness 8)))

;; Test 13: 240-120 degrees (wrap around), thickness 10, radius 45
(define test13 (img-circle-segment img600x400 50 50 45 240 120 13 '(thickness 10)))

;; Test 14: 300-60 degrees (wrap around), thickness 5, radius 55
(define test14 (img-circle-segment img600x400 150 50 55 300 60 14 '(thickness 5)))

;; Test 15: 225-135 degrees (wrap around), thickness 12, radius 40
(define test15 (img-circle-segment img600x400 250 50 40 225 135 15 '(thickness 12)))

;; Test 16: 285-105 degrees (wrap around), thickness 7, radius 65
(define test16 (img-circle-segment img600x400 350 50 65 285 105 1 '(thickness 7)))

;; Test 17: 330-30 degrees (wrap around), thickness 15, radius 35
(define test17 (img-circle-segment img600x400 450 50 35 330 30 2 '(thickness 15)))

;; Test edge case: start angle exactly 180 degrees
;; Test 18: 180-90 degrees (wrap around), thickness 8, radius 50
(define test18 (img-circle-segment img600x400 550 50 50 180 90 3 '(thickness 8)))

;; Test 19: 190-80 degrees (wrap around), thickness 6, radius 55
(define test19 (img-circle-segment img600x400 50 150 55 190 80 4 '(thickness 6)))

;; Test 20: 200-70 degrees (wrap around), thickness 9, radius 45
(define test20 (img-circle-segment img600x400 150 150 45 200 70 5 '(thickness 9)))

;; Test specific radii that might trigger edge cases (61, 62, 63)
;; Test 21: Radius 61, 170-30 degrees (line 1415 case)
(define test21 (img-circle-segment img600x400 250 150 61 170 30 6 '(thickness 8)))

;; Test 22: Radius 62, 270-90 degrees (line 1428 case)
(define test22 (img-circle-segment img600x400 350 150 62 270 90 7 '(thickness 6)))

;; Test 23: Radius 63, 150-60 degrees (line 1415 case)
(define test23 (img-circle-segment img600x400 450 150 63 150 60 8 '(thickness 10)))

;; Test larger wrap-around angles
;; Test 24: 300-120 degrees (180 degree wrap), thickness 6, radius 50
(define test24 (img-circle-segment img600x400 550 150 50 300 120 9 '(thickness 6)))

;; Test 25: 135-315 degrees (180 degree wrap, unusual direction), thickness 8, radius 45
(define test25 (img-circle-segment img600x400 50 250 45 135 315 10 '(thickness 8)))

;; Test with rounded ends (might affect cap line calculations)
;; Test 26: 170-30 degrees, thickness 8, radius 50, rounded (line 1415)
(define test26 (img-circle-segment img600x400 150 250 50 170 30 11 '(rounded) '(thickness 8)))

;; Test 27: 270-90 degrees, thickness 10, radius 45, rounded (line 1428)
(define test27 (img-circle-segment img600x400 250 250 45 270 90 12 '(rounded) '(thickness 10)))

;; Test small wrap-around angles
;; Test 28: 10-350 degrees (small wrap), thickness 6, radius 50
(define test28 (img-circle-segment img600x400 350 250 50 10 350 13 '(thickness 6)))

;; Test 29: 190-170 degrees (small wrap), thickness 8, radius 45
(define test29 (img-circle-segment img600x400 450 250 45 190 170 14 '(thickness 8)))

;; Test very thick segments that should definitely create split slices
;; Test 30: 160-40 degrees, thickness 20, radius 50 (line 1415)
(define test30 (img-circle-segment img600x400 550 250 50 160 40 15 '(thickness 20)))

;; Test 31: 280-80 degrees, thickness 18, radius 55 (line 1428)
(define test31 (img-circle-segment img600x400 50 350 55 280 80 1 '(thickness 18)))

;; Test 32: 120-240 degrees, thickness 16, radius 45 (unusual wrap)
(define test32 (img-circle-segment img600x400 150 350 45 120 240 2 '(thickness 16)))

;; Test specific fractional angles
;; Test 33: 157.5-22.5 degrees (line 1415 case), thickness 6, radius 50
(define test33 (img-circle-segment img600x400 250 350 50 157.5 22.5 3 '(thickness 6)))

;; Test 34: 247.5-112.5 degrees (line 1428 case), thickness 8, radius 45
(define test34 (img-circle-segment img600x400 350 350 45 247.5 112.5 4 '(thickness 8)))

;; Test edge boundary cases
;; Test 35: 179.9-0.1 degrees (just under 180), thickness 7, radius 52
(define test35 (img-circle-segment img600x400 450 350 52 179.9 0.1 5 '(thickness 7)))

;; Test 36: 180.1-179.9 degrees (just over 180), thickness 7, radius 52  
(define test36 (img-circle-segment img600x400 550 350 52 180.1 179.9 6 '(thickness 7)))

;; Test error conditions (should not crash)
;; Test 37: Invalid image buffer
(define test37 (trap (img-circle-segment "invalid" 100 100 50 270 90 1 '(thickness 6))))

;; Test 38: Invalid color
(define test38 (trap (img-circle-segment img600x400 100 100 50 270 90 "invalid" '(thickness 6))))

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
         test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
         test31 test32 test33 test34 test35 test36
         (eq test37 '(exit-error type_error))
         (eq test38 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))