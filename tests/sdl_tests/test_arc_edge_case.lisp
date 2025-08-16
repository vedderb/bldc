
(sdl-init)
 
(define win (sdl-create-window "Arc Test" 400 400))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

(sdl-set-active-renderer rend) ;; Connect the renderer to the display library

(define img (img-buffer 'indexed16 400 400))

(def angle0 300)
(def angle1 298)

(img-clear img)
(define r1 (img-arc img
                    200 200
                    180
                    260 270
                    2
                    '(thickness 40)))
(define r2 (img-arc img
                    200 200
                    180
                    270 260
                    3
                    '(thickness 40)))
(define r3 (img-arc img
                    200 200
                    180
                    300 298
                    1
                    '(thickness 20)))
(define r4 (img-arc img
                    200 200
                    180
                    30 35
                    1
                    '(thickness 20)))
(define r5 (img-arc img
                    200 200
                    180
                    35 30
                    1
                    '(thickness 20)))

(disp-render img 0 0 (list 0x0 0xff5f5f 0x00ff5f 0x5f00ff))

(if (and r1 r2 r3 r4 r5)
    (print "SUCCESS")
    (print "FAILURE"))


