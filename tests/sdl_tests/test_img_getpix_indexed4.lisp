(sdl-init)

(define win (sdl-create-window "GetPixel Test - indexed4" 400 200))
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

(define img400x200 (img-buffer 'indexed4 400 200))

;; Clear the image buffer to ensure known starting state
(img-clear img400x200 0)

;; Test 1: Get pixel from cleared buffer (should be 0)
(define test1 (= (img-getpix img400x200 100 100) 0))

;; Test 2: Set and get pixel at origin
(img-setpix img400x200 0 0 1)
(define test2 (= (img-getpix img400x200 0 0) 1))

;; Test 3: Set and get pixel at center
(img-setpix img400x200 200 100 2)
(define test3 (= (img-getpix img400x200 200 100) 2))

;; Test 4: Set and get pixel at corner
(img-setpix img400x200 399 199 3)
(define test4 (= (img-getpix img400x200 399 199) 3))

;; Test 5: Get pixel outside bounds (should return 0 or handle gracefully)
(define test5 (img-getpix img400x200 400 200))
(define test6 (img-getpix img400x200 -1 -1))

;; Test 7-10: Set and verify all indexed4 colors (0-3)
(img-setpix img400x200 10 10 0)
(define test7 (= (img-getpix img400x200 10 10) 0))

(img-setpix img400x200 20 10 1)
(define test8 (= (img-getpix img400x200 20 10) 1))

(img-setpix img400x200 30 10 2)
(define test9 (= (img-getpix img400x200 30 10) 2))

(img-setpix img400x200 40 10 3)
(define test10 (= (img-getpix img400x200 40 10) 3))

;; Test 11: Set with color outside range and verify clipping/wrapping
(img-setpix img400x200 50 10 4)
(define test11_val (img-getpix img400x200 50 10))
(define test11 (or (= test11_val 0) (= test11_val 1) (= test11_val 2) (= test11_val 3)))

;; Test 12: Set with very large color and verify clipping
(img-setpix img400x200 60 10 255)
(define test12_val (img-getpix img400x200 60 10))
(define test12 (or (= test12_val 0) (= test12_val 1) (= test12_val 2) (= test12_val 3)))

;; Test 13: Verify neighboring pixels are not affected
(img-setpix img400x200 100 50 2)
(define test13a (= (img-getpix img400x200 100 50) 2))    ; Set pixel
(define test13b (= (img-getpix img400x200 99 50) 0))     ; Left neighbor should be 0
(define test13c (= (img-getpix img400x200 101 50) 0))    ; Right neighbor should be 0
(define test13d (= (img-getpix img400x200 100 49) 0))    ; Top neighbor should be 0
(define test13e (= (img-getpix img400x200 100 51) 0))    ; Bottom neighbor should be 0
(define test13 (and test13a test13b test13c test13d test13e))

;; Test 14: Test with invalid image buffer
(define test14 (trap (img-getpix "not-an-image" 100 100)))
(define test15 (trap (img-getpix nil 100 100)))

;; Test 16: Test with non-numeric coordinates
(define test16 (trap (img-getpix img400x200 "x" 100)))
(define test17 (trap (img-getpix img400x200 100 "y")))

;; Test 18: Multiple set/get operations to verify consistency
(img-setpix img400x200 150 75 1)
(img-setpix img400x200 151 75 2)
(img-setpix img400x200 152 75 3)
(define test18a (= (img-getpix img400x200 150 75) 1))
(define test18b (= (img-getpix img400x200 151 75) 2))
(define test18c (= (img-getpix img400x200 152 75) 3))
(define test18 (and test18a test18b test18c))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF 0xFF0000 0x00FF00))

;; Check test results
(if (and test1 test2 test3 test4 test7 test8 test9 test10 test11 test12 test13 test18
         (eq test14 '(exit-error type_error))
         (eq test15 '(exit-error type_error))
         (eq test16 '(exit-error type_error))
         (eq test17 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))