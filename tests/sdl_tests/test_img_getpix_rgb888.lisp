(sdl-init)

(define win (sdl-create-window "GetPixel Test - rgb888" 400 200))
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

(define img400x200 (img-buffer 'rgb888 400 200))

;; Clear the image buffer to ensure known starting state
(img-clear img400x200 0)

;; Test 1: Get pixel from cleared buffer (should be 0)
(define test1 (= (img-getpix img400x200 100 100) 0))

;; Test 2: Set and get exact rgb888 values (should match exactly - no quantization)
(define red888 0xFF0000)     ; Pure red
(define green888 0x00FF00)   ; Pure green  
(define blue888 0x0000FF)    ; Pure blue
(define white888 0xFFFFFF)   ; White
(define black888 0x000000)   ; Black

(img-setpix img400x200 0 0 red888)
(define test2 (= (img-getpix img400x200 0 0) red888))

(img-setpix img400x200 10 0 green888)
(define test3 (= (img-getpix img400x200 10 0) green888))

(img-setpix img400x200 20 0 blue888)
(define test4 (= (img-getpix img400x200 20 0) blue888))

(img-setpix img400x200 30 0 white888)
(define test5 (= (img-getpix img400x200 30 0) white888))

(img-setpix img400x200 40 0 black888)
(define test6 (= (img-getpix img400x200 40 0) black888))

;; Test 7: Test various rgb888 values (should all match exactly)
(define orange888 0xFF8000)    ; Orange
(define purple888 0x800080)    ; Purple
(define gray888 0x808080)      ; Gray
(define yellow888 0xFFFF00)    ; Yellow
(define cyan888 0x00FFFF)      ; Cyan
(define magenta888 0xFF00FF)   ; Magenta

(img-setpix img400x200 50 10 orange888)
(define test7 (= (img-getpix img400x200 50 10) orange888))

(img-setpix img400x200 60 10 purple888)
(define test8 (= (img-getpix img400x200 60 10) purple888))

(img-setpix img400x200 70 10 gray888)
(define test9 (= (img-getpix img400x200 70 10) gray888))

(img-setpix img400x200 80 10 yellow888)
(define test10 (= (img-getpix img400x200 80 10) yellow888))

(img-setpix img400x200 90 10 cyan888)
(define test11 (= (img-getpix img400x200 90 10) cyan888))

(img-setpix img400x200 100 10 magenta888)
(define test12 (= (img-getpix img400x200 100 10) magenta888))

;; Test 13: Test arbitrary values should match exactly
(define arbitrary1 0x123456)
(define arbitrary2 0xABCDEF)
(define arbitrary3 0x555555)

(img-setpix img400x200 110 10 arbitrary1)
(define test13 (= (img-getpix img400x200 110 10) arbitrary1))

(img-setpix img400x200 120 10 arbitrary2)
(define test14 (= (img-getpix img400x200 120 10) arbitrary2))

(img-setpix img400x200 130 10 arbitrary3)
(define test15 (= (img-getpix img400x200 130 10) arbitrary3))

;; Test 16: Get pixel outside bounds
(define test16 (img-getpix img400x200 400 200))
(define test17 (img-getpix img400x200 -1 -1))

;; Test 18: Verify neighboring pixels are not affected
(img-setpix img400x200 200 50 0x556677)
(define test18a (= (img-getpix img400x200 200 50) 0x556677))  ; Set pixel
(define test18b (= (img-getpix img400x200 199 50) 0))         ; Left neighbor should be 0
(define test18c (= (img-getpix img400x200 201 50) 0))         ; Right neighbor should be 0
(define test18d (= (img-getpix img400x200 200 49) 0))         ; Top neighbor should be 0
(define test18e (= (img-getpix img400x200 200 51) 0))         ; Bottom neighbor should be 0
(define test18 (and test18a test18b test18c test18d test18e))

;; Test 19: Test with invalid image buffer
(define test19 (trap (img-getpix "not-an-image" 100 100)))
(define test20 (trap (img-getpix nil 100 100)))

;; Test 21: Test with non-numeric coordinates
(define test21 (trap (img-getpix img400x200 "x" 100)))
(define test22 (trap (img-getpix img400x200 100 "y")))

;; Test 23: Multiple set/get operations to verify consistency
(img-setpix img400x200 150 75 red888)
(img-setpix img400x200 151 75 green888)
(img-setpix img400x200 152 75 blue888)
(define test23a (= (img-getpix img400x200 150 75) red888))
(define test23b (= (img-getpix img400x200 151 75) green888))
(define test23c (= (img-getpix img400x200 152 75) blue888))
(define test23 (and test23a test23b test23c))

;; Test 24: Test edge values (min/max for each component)
(define max_red 0xFF0000)
(define max_green 0x00FF00)
(define max_blue 0x0000FF)
(define min_color 0x000000)
(define max_color 0xFFFFFF)

(img-setpix img400x200 300 100 max_red)
(define test24a (= (img-getpix img400x200 300 100) max_red))

(img-setpix img400x200 310 100 max_green)
(define test24b (= (img-getpix img400x200 310 100) max_green))

(img-setpix img400x200 320 100 max_blue)
(define test24c (= (img-getpix img400x200 320 100) max_blue))

(img-setpix img400x200 330 100 min_color)
(define test24d (= (img-getpix img400x200 330 100) min_color))

(img-setpix img400x200 340 100 max_color)
(define test24e (= (img-getpix img400x200 340 100) max_color))

(define test24 (and test24a test24b test24c test24d test24e))

;; Display the result
(disp-render img400x200 0 0)

;; Check test results - all should work perfectly for rgb888
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10 test11 test12 
         test13 test14 test15 test18 test23 test24
         (eq test19 '(exit-error type_error))
         (eq test20 '(exit-error type_error))
         (eq test21 '(exit-error type_error))
         (eq test22 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))