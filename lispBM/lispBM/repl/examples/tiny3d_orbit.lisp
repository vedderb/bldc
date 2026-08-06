; tiny3d orbit demo: a few static cubes in the world, camera orbiting
; around the world's up/down (Y) axis, always facing the scene center.

(sdl-init)

(define win (sdl-create-window "tiny3d orbit demo" 640 480))
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

(def img (img-buffer 'rgb888 640 480))

; near, far, vertical-fov-degrees, cull-margin (world units), max
; triangles per object (36 is plenty for these 12-triangle cubes).
(def state (tiny3d-state-create img 64 0.5 100.0 60.0 0.5 '(filled)))

; Cube, side 2, centered at local origin: 8 shared vertices, 12 triangles
; referencing them by index (rather than each triangle carrying its own
; duplicated 3 corners). Windings verified against tiny3d.c's
; front-facing convention (see the tiny3d cube demo).
(def cube-verts (list
  (list -1.0 -1.0 -1.0)   ; 0
  (list  1.0 -1.0 -1.0)   ; 1
  (list  1.0  1.0 -1.0)   ; 2
  (list -1.0  1.0 -1.0)   ; 3
  (list -1.0 -1.0  1.0)   ; 4
  (list  1.0 -1.0  1.0)   ; 5
  (list  1.0  1.0  1.0)   ; 6
  (list -1.0  1.0  1.0))) ; 7

(defun cube-tris (color) (list
  (list 4 5 6 color) (list 4 6 7 color)   ; front  z=+1
  (list 0 2 1 color) (list 0 3 2 color)   ; back   z=-1
  (list 1 2 6 color) (list 1 6 5 color)   ; right  x=+1
  (list 0 7 3 color) (list 0 4 7 color)   ; left   x=-1
  (list 3 6 2 color) (list 3 7 6 color)   ; top    y=+1
  (list 0 1 5 color) (list 0 5 4 color))) ; bottom y=-1

(defun cube-mesh (color) (tiny3d-mesh cube-verts (cube-tris color)))

; A handful of static objects in the world - only the camera moves.
(def objects (list
  (tiny3d-instance (cube-mesh 0xE04030) (list -2.5  0.0  8.0) (list  0.0  20.0   0.0))
  (tiny3d-instance (cube-mesh 0x30C060) (list  0.0  1.2  9.5) (list  15.0 45.0   0.0))
  (tiny3d-instance (cube-mesh 0x3080E0) (list  2.5 -0.7  7.5) (list  0.0  70.0  10.0))
  (tiny3d-instance (cube-mesh 0xE0C030) (list  0.0 -1.5 11.0) (list  0.0  0.0    0.0))))

; Split pipeline: cull each object (stage 1, tiny3d-cull), sort the
; survivors far-to-near by depth using LispBM's built-in sort, then
; render the sorted list (stage 2, tiny3d-render already does its own
; cull internally too - cheap and harmless when re-run on objects
; already known visible).
(defun cull-and-collect (objects cam-pos cam-orient)
  (if (eq objects nil)
      nil
      (let ((obj (car objects))
            (remaining (cull-and-collect (cdr objects) cam-pos cam-orient)))
        (let ((depth (tiny3d-cull state obj cam-pos cam-orient)))
          (if depth (cons (cons obj depth) remaining) remaining)))))

(defun strip-depth (lst)
  (if (eq lst nil) nil (cons (car (car lst)) (strip-depth (cdr lst)))))

(defun render-sorted (objects cam-pos cam-orient) {
  (var visible (cull-and-collect objects cam-pos cam-orient))
  (var sorted (sort (fn (a b) (> (cdr a) (cdr b))) visible))
  (tiny3d-render state (strip-depth sorted) cam-pos cam-orient)
})

; Scene center the camera orbits around and always looks at.
(def center-x 0.0)
(def center-y 0.0)
(def center-z 9.0)
(def orbit-radius 7.0)

(def theta 0.0)
(def fps 0)
; dt-driven, not fixed-per-frame: theta advances by angular-velocity*dt
; using the *previous* frame's measured duration, so the orbit speed
; stays constant in real time regardless of how long any given frame
; takes to compute (e.g. an occasional GC pause) - a fixed per-frame
; step combined with a plain (sleep ...) let variable frame time turn
; into visible stutter, since equal angle over unequal time isn't
; constant angular velocity.
(def dt 0.016)
(def angular-velocity 0.75) ; radians/second

(loopwhile t {
        (var t-start (systime))

        (img-clear img 0x101018)

        ; Camera position traces a circle around center in the XZ plane
        ; (orbiting the Y/up-down axis); yaw = theta keeps it facing
        ; center - see tiny3d.c's rotation_y3x4 for why this angle is
        ; exactly the one that makes local +z point back at the center.
        ; cam_pos = center - R*(sin theta, 0, cos theta): both components
        ; subtracted.
        (atomic
         (setq theta (+ theta (* angular-velocity dt))))
        (var cam-x (- center-x (* orbit-radius (sin theta))))
        (var cam-z (- center-z (* orbit-radius (cos theta))))
        (var cam-pos (list cam-x center-y cam-z))
        (var yaw-deg (/ (* theta 180.0) 3.14159265))
        (var cam-orient (list 0.0 yaw-deg 0.0))

        (render-sorted objects cam-pos cam-orient)

        (img-text img 5 5 0xffffff 0x101018 (str-from-n fps "FPS %.1f"))
        (disp-render img 0 0)

        (sleep 0.001)
        (setq dt (secs-since t-start))
        (setq fps (/ 1.0 dt))
}))
