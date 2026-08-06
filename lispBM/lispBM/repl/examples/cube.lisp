
(sdl-init)

(define win (sdl-create-window "Rotating cube" 400 400))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

(sdl-set-active-renderer rend)

(define img (img-buffer 'indexed16 400 400))

;; index 0 = background, 1-6 = the six faces, 7 = face edge outline
(def face-colors '(0x000000 0xE04030 0x30C060 0x3080E0 0xE0C030 0xC030E0 0x30E0C0 0x303030))

;; Nodes and faces of a 3d cube.
(def nodes '((-1 -1 -1) (-1 -1 1) (-1 1 -1) (-1 1 1) (1 -1 -1) (1 -1 1) (1 1 -1) (1 1 1)))
(def faces (list
             (list 0 1 3 2)
             (list 4 6 7 5)
             (list 0 4 5 1)
             (list 2 3 7 6)
             (list 0 2 6 4)
             (list 1 5 7 3)))

(defun project (n scale ofs-x ofs-y)
  (list (to-i (* (+ ofs-x (ix n 0)) scale))
        (to-i (* (+ ofs-y (ix n 1)) scale))))

(defun signed-area (p0 p1 p2)
  (- (* (- (ix p1 0) (ix p0 0)) (- (ix p2 1) (ix p0 1)))
     (* (- (ix p1 1) (ix p0 1)) (- (ix p2 0) (ix p0 0)))))

(defun draw-quad (p0 p1 p2 p3 color)
  {
   (img-triangle img (ix p0 0) (ix p0 1) (ix p1 0) (ix p1 1) (ix p2 0) (ix p2 1) color '(filled))
   (img-triangle img (ix p0 0) (ix p0 1) (ix p2 0) (ix p2 1) (ix p3 0) (ix p3 1) color '(filled))
   (img-line img (ix p0 0) (ix p0 1) (ix p1 0) (ix p1 1) 7 '(thickness 4))
   (img-line img (ix p1 0) (ix p1 1) (ix p2 0) (ix p2 1) 7 '(thickness 4))
   (img-line img (ix p2 0) (ix p2 1) (ix p3 0) (ix p3 1) 7 '(thickness 4))
   (img-line img (ix p3 0) (ix p3 1) (ix p0 0) (ix p0 1) 7 '(thickness 4))
   })

(defun draw-faces () {
        (var scale 110.0)
        (var ofs-x (/ 200.0 scale))
        (var ofs-y (/ 180.0 scale))
        (var face-i 0)

        (loopforeach f faces {
                (var p0 (project (ix nodes (ix f 0)) scale ofs-x ofs-y))
                (var p1 (project (ix nodes (ix f 1)) scale ofs-x ofs-y))
                (var p2 (project (ix nodes (ix f 2)) scale ofs-x ofs-y))
                (var p3 (project (ix nodes (ix f 3)) scale ofs-x ofs-y))

                (if (> (signed-area p0 p1 p2) 0)
                    (draw-quad p0 p1 p2 p3 (+ face-i 1))
                    nil)
                (setq face-i (+ face-i 1))
        })
})

(defun rotate-cube (ax ay) {
        (var sx (sin ax))
        (var cx (cos ax))
        (var sy (sin ay))
        (var cy (cos ay))

        (loopforeach n nodes {
                (var x (ix n 0))
                (var y (ix n 1))
                (var z (ix n 2))

                (setix n 0 (- (* x cx) (* z sx)))
                (setix n 2 (+ (* z cx) (* x sx)))
                (setvar 'z (ix n 2))
                (setix n 1 (- (* y cy) (* z sy)))
                (setix n 2 (+ (* z cy) (* y sy)))
        })
})

(def fps 0)

(loopwhile t {
        (var t-start (systime))
        (img-clear img 0)
        (draw-faces)
        (rotate-cube 0.1 0.05)
        (img-text img 5 370 1 0 (str-from-n fps "FPS %.1f") '(magnify 2))
        (disp-render img 0 0 face-colors)
        (sleep 0.01)
        (setq fps (/ 1 (secs-since t-start)))
})
