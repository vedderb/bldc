(sdl-init)

(define win (sdl-create-window "TTF Prepare Test" 400 200))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

;; Load a TTF font for valid tests
(define font-file (fopen "./sdl_tests/Ubuntu-Regular.ttf" "r"))
(define font-data (load-file font-file))

;; Connect the renderer to the display library
(sdl-set-active-renderer rend)

;; Test valid ttf-prepare calls

;; Test different color formats
(define r1 (ttf-prepare font-data 16 'indexed2 "Hello"))
(define r2 (ttf-prepare font-data 16 'indexed4 "World"))
(define r3 (ttf-prepare font-data 16 'rgb888 "Test"))

;; Test different font sizes
(define r4 (ttf-prepare font-data 8 'indexed2 "Small"))
(define r5 (ttf-prepare font-data 32 'indexed2 "Large"))

;; Test minimal character set
(define r6 (ttf-prepare font-data 16 'indexed2 "A"))

;; Test special characters
(define r7 (ttf-prepare font-data 16 'indexed2 "!@#$%"))

;; Test edge cases that work
(define r8 (ttf-prepare font-data 0 'indexed2 "Zero"))   ;; Zero scale works
(define r9 (ttf-prepare font-data -1 'indexed2 "Neg"))  ;; Negative scale works

;; Test error conditions
(define e1 (trap (ttf-prepare "invalid" 16 'indexed2 "Hello")))     ;; Invalid font
(define e2 (trap (ttf-prepare font-data 16 'invalid-fmt "Hello")))  ;; Invalid format
(define e3 (trap (ttf-prepare font-data 16 'indexed2 "")))          ;; Empty string
(define e4 (trap (ttf-prepare font-data 16 'indexed2)))             ;; Missing args

;; Verify results
(define valid_calls_work (and 
  (not (eq r1 nil)) (not (eq r2 nil)) (not (eq r3 nil))
  (not (eq r4 nil)) (not (eq r5 nil)) (not (eq r6 nil))
  (not (eq r7 nil)) (not (eq r8 nil)) (not (eq r9 nil))))

(define error_cases_fail (and
  (eq e1 '(exit-error eval_error))
  (eq e2 '(exit-error eval_error))
  (eq e3 '(exit-error eval_error))
  (eq e4 '(exit-error type_error))))

(if (and valid_calls_work error_cases_fail)
    (print "SUCCESS")
    (print "FAILURE"))