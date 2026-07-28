(def tab (wasm-create-tab "my drawing"))
(def canvas (wasm-add-canvas tab 320 240))
(def img (img-buffer 'rgb888 320 240))
(img-clear img 0x000000)

(define speed 1.0)
(define increment 0.01)

(defun do-up () (setq speed (+ speed increment)))
(defun do-down () (setq speed (- speed increment)))


(define kb (wasm-add-keyboard-control tab "Keyboard"))
(wasm-keyboard-control-bind kb "ArrowUp"   "(do-up)"   "")
(wasm-keyboard-control-bind kb "ArrowDown" "(do-down)"  "")

; Nodes and edges of a 3d cube
(def nodes '((-1 -1 -1) (-1 -1 1) (-1 1 -1) (-1 1 1) (1 -1 -1) (1 -1 1) (1 1 -1) (1 1 1)))
(def edges '((0  1) (1 3) (3 2) (2 0) (4 5) (5 7) (7 6) (6 4) (0 4) (1 5) (2 6) (3 7)))

(defun line (x1 y1 x2 y2)
       (img-line img x1 y1 x2 y2 0xFFFFFF)
       )

(defun draw-edges () {
        (var scale 50.0)
        (var ofs-x (/ 160 scale))
        (var ofs-y (/ 120 scale))

        (loopforeach e edges {
                (var na (ix nodes (ix e 0)))
                (var nb (ix nodes (ix e 1)))

                (apply line (map (fn (x) (to-i (* x scale))) (list
                            (+ ofs-x (ix na 0)) (+ ofs-y (ix na 1))
                            (+ ofs-x (ix nb 0)) (+ ofs-y (ix nb 1))
                )))
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

(loopwhile t {
        (draw-edges)
        (rotate-cube (* 0.1 speed) (* 0.05 speed))
        (disp-render img 0 0 '())
        (img-clear img)
        (sleep 0.04)
})
