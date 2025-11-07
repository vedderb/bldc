(sdl-init)

(define win (sdl-create-window "GetPixel Test - indexed16" 400 200))
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

;; Clear the image buffer to ensure known starting state
(img-clear img400x200 0)

;; Test 1: Get pixel from cleared buffer (should be 0)
(define test1 (= (img-getpix img400x200 100 100) 0))

;; Test 2: Set and get pixel at origin
(img-setpix img400x200 0 0 1)
(define test2 (= (img-getpix img400x200 0 0) 1))

;; Test 3: Set and get pixel at center
(img-setpix img400x200 200 100 8)
(define test3 (= (img-getpix img400x200 200 100) 8))

;; Test 4: Set and get pixel at corner
(img-setpix img400x200 399 199 15)
(define test4 (= (img-getpix img400x200 399 199) 15))

;; Test 5: Get pixel outside bounds (should return 0 or handle gracefully)
(define test5 (img-getpix img400x200 400 200))
(define test6 (img-getpix img400x200 -1 -1))

;; Test 7-22: Set and verify all indexed16 colors (0-15)
(img-setpix img400x200 10 10 0)
(define test7 (= (img-getpix img400x200 10 10) 0))

(img-setpix img400x200 20 10 1)
(define test8 (= (img-getpix img400x200 20 10) 1))

(img-setpix img400x200 30 10 2)
(define test9 (= (img-getpix img400x200 30 10) 2))

(img-setpix img400x200 40 10 3)
(define test10 (= (img-getpix img400x200 40 10) 3))

(img-setpix img400x200 50 10 4)
(define test11 (= (img-getpix img400x200 50 10) 4))

(img-setpix img400x200 60 10 5)
(define test12 (= (img-getpix img400x200 60 10) 5))

(img-setpix img400x200 70 10 6)
(define test13 (= (img-getpix img400x200 70 10) 6))

(img-setpix img400x200 80 10 7)
(define test14 (= (img-getpix img400x200 80 10) 7))

(img-setpix img400x200 90 10 8)
(define test15 (= (img-getpix img400x200 90 10) 8))

(img-setpix img400x200 100 10 9)
(define test16 (= (img-getpix img400x200 100 10) 9))

(img-setpix img400x200 110 10 10)
(define test17 (= (img-getpix img400x200 110 10) 10))

(img-setpix img400x200 120 10 11)
(define test18 (= (img-getpix img400x200 120 10) 11))

(img-setpix img400x200 130 10 12)
(define test19 (= (img-getpix img400x200 130 10) 12))

(img-setpix img400x200 140 10 13)
(define test20 (= (img-getpix img400x200 140 10) 13))

(img-setpix img400x200 150 10 14)
(define test21 (= (img-getpix img400x200 150 10) 14))

(img-setpix img400x200 160 10 15)
(define test22 (= (img-getpix img400x200 160 10) 15))

;; Test 23: Set with color outside range and verify clipping/wrapping
(img-setpix img400x200 170 10 16)
(define test23_val (img-getpix img400x200 170 10))
(define test23 (and (>= test23_val 0) (<= test23_val 15)))

;; Test 24: Set with very large color and verify clipping
(img-setpix img400x200 180 10 255)
(define test24_val (img-getpix img400x200 180 10))
(define test24 (and (>= test24_val 0) (<= test24_val 15)))

;; Test 25: Verify neighboring pixels are not affected
(img-setpix img400x200 100 50 7)
(define test25a (= (img-getpix img400x200 100 50) 7))    ; Set pixel
(define test25b (= (img-getpix img400x200 99 50) 0))     ; Left neighbor should be 0
(define test25c (= (img-getpix img400x200 101 50) 0))    ; Right neighbor should be 0
(define test25d (= (img-getpix img400x200 100 49) 0))    ; Top neighbor should be 0
(define test25e (= (img-getpix img400x200 100 51) 0))    ; Bottom neighbor should be 0
(define test25 (and test25a test25b test25c test25d test25e))

;; Test 26: Test with invalid image buffer
(define test26 (trap (img-getpix "not-an-image" 100 100)))
(define test27 (trap (img-getpix nil 100 100)))

;; Test 28: Test with non-numeric coordinates
(define test28 (trap (img-getpix img400x200 "x" 100)))
(define test29 (trap (img-getpix img400x200 100 "y")))

;; Test 30: Multiple set/get operations to verify consistency
(img-setpix img400x200 150 75 5)
(img-setpix img400x200 151 75 10)
(img-setpix img400x200 152 75 15)
(define test30a (= (img-getpix img400x200 150 75) 5))
(define test30b (= (img-getpix img400x200 151 75) 10))
(define test30c (= (img-getpix img400x200 152 75) 15))
(define test30 (and test30a test30b test30c))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 
                               0xFFFF00 0xFF00FF 0x00FFFF 0xFFFFFF
                               0x800000 0x008000 0x000080 0x808000
                               0x800080 0x008080 0x808080 0xC0C0C0))

;; Check test results
(if (and test1 test2 test3 test4 test7 test8 test9 test10 test11 test12 test13 test14 
         test15 test16 test17 test18 test19 test20 test21 test22 test23 test24 test25 test30
         (eq test26 '(exit-error type_error))
         (eq test27 '(exit-error type_error))
         (eq test28 '(exit-error type_error))
         (eq test29 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))