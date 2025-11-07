(sdl-init)

(define win (sdl-create-window "TTF Metrics Test" 400 200))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

;; Load a TTF font
(define font-file (fopen "./sdl_tests/Ubuntu-Regular.ttf" "r"))
(define font-data (load-file font-file))
(define font (ttf-prepare font-data 32 'indexed4 " HeloWrdTts123!@#EgfCrnx"))

;; Connect the renderer to the display library
(sdl-set-active-renderer rend)

;; Test core TTF metrics functions
(define line_height (ttf-line-height font))
(define ascender (ttf-ascender font))
(define descender (ttf-descender font))
(define line_gap (ttf-line-gap font))

;; Test text and glyph dimensions
(define text_dims (ttf-text-dims font "Hello"))
(define glyph_dims (ttf-glyph-dims font "H"))

;; Test basic error handling
(define err1 (trap (ttf-line-height "invalid")))
(define err2 (trap (ttf-text-dims "invalid" "text")))

;; Simple validations
(define metrics_work (and 
  (> line_height 0.0f32) 
  (> ascender 0.0f32) 
  (< descender 0.0f32) 
  (>= line_gap 0.0f32)))

(define dims_work (and 
  (list? text_dims) (eq (length text_dims) 2)
  (list? glyph_dims) (eq (length glyph_dims) 2)))

(define errors_work (and
  (eq err1 '(exit-error eval_error))
  (eq err2 '(exit-error eval_error))))

(if (and metrics_work dims_work errors_work)
    (print "SUCCESS")
    (print "FAILURE"))