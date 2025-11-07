(hide-trapped-error)
(if (not (eq (trap sdl-init)
             '(exit-ok sdl-init)))
    {
    (print "You need a LispBM REPL compiled with SDL support to run this application.")
    (exit-error 'no-sdl)
    }
    )

(define demo-running t)

(defun event-loop (w)
  (let ((event-loop-running t))
    (loopwhile event-loop-running
          {
          (match (sdl-poll-event)
                 (sdl-quit-event
                  {
                  (custom-destruct w)
                  (setq demo-running nil)
                  })
                 ((sdl-key-down-event . (? k))
                  nil) ;; Do not process keydown yet
                 ((sdl-key-up-event . (? k))
                  nil) ;; Do not process keyup yet
                 (_ nil) ;; unused command
                 )
          (yield 5000)
          })))


;; Start the game 
(sdl-init)
(define win (sdl-create-window "DEMO" 400 400))
(define rend (sdl-create-soft-renderer win))

(sdl-renderer-set-color rend 0 0 0)
(sdl-clear rend)
(sdl-renderer-set-color rend 255 255 255)
(sdl-set-active-renderer rend) ;; Connect the renderer to the display library

(define font-file (fopen "Ubuntu-Regular.ttf" "r"))
(define font (load-file font-file))
(define ttf (ttf-prepare font 32 'indexed4 "abcdefghijklmnopqrstuvxyz1234567890+-*/"))
(define aa-text '(0 8 1 1))

(define disp (img-buffer 'indexed16 400 400))

(spawn 100 event-loop win)


(defun max (a b) (if (> a b) a b))
(defun min (a b) (if (< a b) a b))

(defun apply-max (lst)
  (let ((result (first lst)))
    {
    (map (lambda (x) (if (> x result) (setq result x) nil)) lst)
    result
    }))

;; Camera system
(define camera-x 0)       ;; World x position of camera
(define camera-target 0)  ;; Target x position for smooth following

;; Dynamic environment that gets populated as we scroll
(define environment '())

;; Generate environment segments for scrolling
(defun generate-environment-segment (start-x)
  {
  ;; Generate a ground segment
  (let ((y1 (+ 280 (* 20 (sin (* start-x 0.01)))))  ;; Wavy ground
        (y2 (+ 280 (* 20 (sin (* (+ start-x 100) 0.01))))))
    (list start-x y1 (+ start-x 100) y2))
  })

;; Update environment based on camera position
(defun update-environment ()
  {
  ;; Clear old segments that are too far behind camera
  (setq environment (filter (lambda (seg) (> (ix seg 2) (- camera-x 100))) environment))
  
  ;; Add new segments ahead of camera
  (let ((rightmost-x (if (eq environment '()) 
                        (- camera-x 200)
                        (apply-max (map (lambda (seg) (ix seg 2)) environment)))))
    (loopwhile (< rightmost-x (+ camera-x 600))
     {
      (setq environment (cons (generate-environment-segment rightmost-x) environment))
      (setq rightmost-x (+ rightmost-x 100))
      }))
  })

;; Draw environment line segments with camera offset
(defun draw-environment ()
  {
  (img-clear disp)
  (update-environment)
  (map (lambda (seg)
         (let ((screen-x1 (- (ix seg 0) camera-x))
               (y1 (ix seg 1))  
               (screen-x2 (- (ix seg 2) camera-x))
               (y2 (ix seg 3)))
           ;; Only draw if segment is visible on screen
           (if (and (< screen-x1 450) (> screen-x2 -50))
               (img-line disp screen-x1 y1 screen-x2 y2 1)
               nil)))
       environment)
  })

;; Stick figure data structure: (x y direction state task-id color phase)
;; direction: 1=right, -1=left
;; state: 'running 'idle
;; phase: animation frame counter
(define stick-figures '())

;; Running animation frames with subtle, natural running motion
(defun get-running-frame (phase)
  (let ((frame (mod phase 16))) ;; 16-frame running cycle
    (cond 
      ;; Frame 0-3: Left leg forward stride, right arm forward
      ((< frame 4)
       '(-3 -12 -6 -10   ;; Left arm back (subtle swing)
         6 -12 10 -10    ;; Right arm forward (gentle swing)
         8 3 12 12       ;; Left leg forward (moderate stride)
         -4 6 -6 12))    ;; Right leg back (slight bend)
      
      ;; Frame 4-7: Contact/midstance phase  
      ((< frame 8)
       '(0 -15 2 -12     ;; Left arm neutral
         3 -15 5 -12     ;; Right arm neutral  
         4 6 6 12        ;; Left leg under body
         -2 6 -4 12))    ;; Right leg under body
      
      ;; Frame 8-11: Right leg forward stride, left arm forward
      ((< frame 12)
       '(6 -12 10 -10    ;; Left arm forward (gentle swing)
         -3 -12 -6 -10   ;; Right arm back (subtle swing)
         -4 6 -6 12      ;; Left leg back (slight bend)
         8 3 12 12))     ;; Right leg forward (moderate stride)
      
      ;; Frame 12-15: Transition phase
      (t
       '(3 -15 5 -12     ;; Left arm transitioning
         0 -15 2 -12     ;; Right arm transitioning
         -2 6 -4 12      ;; Left leg transitioning  
         4 6 6 12)))))

;; Joint position indices in frame data:
;; 0,1: left-elbow-x,y  2,3: left-hand-x,y  4,5: right-elbow-x,y  6,7: right-hand-x,y
;; 8,9: left-knee-x,y  10,11: left-foot-x,y  12,13: right-knee-x,y  14,15: right-foot-x,y

;; Draw a stick figure with proper running animation (camera-relative)
(defun draw-stick-figure (fig)
  (let ((world-x (ix fig 0))  ;; World coordinate
        (world-y (ix fig 1))  ;; World coordinate
        (dir (ix fig 2))
        (state (ix fig 3))
        (task-id (ix  fig 4))
        (color-idx (ix fig 5)) ;; Color index (2-15)
        (phase (ix fig 6)))
    {
    ;; Convert world coordinates to screen coordinates
    (let ((screen-x (- world-x camera-x))
          (screen-y world-y))
      {
      ;; Only draw if figure is visible on screen
      (if (and (> screen-x -50) (< screen-x 450))
          {
          ;; Head
          (img-circle disp screen-x (- screen-y 30) 6 color-idx)
          
          ;; Body
          (img-line disp screen-x (- screen-y 24) screen-x (- screen-y 5) color-idx)
          
          (if (eq state 'running)
              {
              ;; Get animation frame data
              (let ((frame-data (get-running-frame (/ phase 3)))) ;; Slower animation
                {
                ;; Left arm (shoulder -> elbow -> hand)
                (let ((left-elbow-x (+ screen-x (ix frame-data 0)))
                      (left-elbow-y (+ screen-y (ix frame-data 1)))
                      (left-hand-x (+ screen-x (ix frame-data 2)))
                      (left-hand-y (+ screen-y (ix frame-data 3))))
                  {
                  (img-line disp screen-x (- screen-y 20) left-elbow-x left-elbow-y color-idx)
                  (img-line disp left-elbow-x left-elbow-y left-hand-x left-hand-y color-idx)
                  })
                
                ;; Right arm (shoulder -> elbow -> hand)
                (let ((right-elbow-x (+ screen-x (ix frame-data 4)))
                      (right-elbow-y (+ screen-y (ix frame-data 5)))
                      (right-hand-x (+ screen-x (ix frame-data 6)))
                      (right-hand-y (+ screen-y (ix frame-data 7))))
                  {
                  (img-line disp screen-x (- screen-y 20) right-elbow-x right-elbow-y color-idx)
                  (img-line disp right-elbow-x right-elbow-y right-hand-x right-hand-y color-idx)
                  })
                
                ;; Left leg (hip -> knee -> foot)
                (let ((left-knee-x (+ screen-x (ix frame-data 8)))
                      (left-knee-y (+ screen-y (ix frame-data 9)))
                      (left-foot-x (+ screen-x (ix frame-data 10)))
                      (left-foot-y (+ screen-y (ix frame-data 11))))
                  {
                  (img-line disp screen-x (- screen-y 5) left-knee-x left-knee-y color-idx)
                  (img-line disp left-knee-x left-knee-y left-foot-x left-foot-y color-idx)
                  })
                
                ;; Right leg (hip -> knee -> foot)
                (let ((right-knee-x (+ screen-x (ix frame-data 12)))
                      (right-knee-y (+ screen-y (ix frame-data 13)))
                      (right-foot-x (+ screen-x (ix frame-data 14)))
                      (right-foot-y (+ screen-y (ix frame-data 15))))
                  {
                  (img-line disp screen-x (- screen-y 5) right-knee-x right-knee-y color-idx)
                  (img-line disp right-knee-x right-knee-y right-foot-x right-foot-y color-idx)
                  })
                })
              }
              {
              ;; Static pose for idle state
              (img-line disp screen-x (- screen-y 20) (- screen-x 10) (- screen-y 12) color-idx)
              (img-line disp screen-x (- screen-y 20) (+ screen-x 10) (- screen-y 12) color-idx)
              (img-line disp screen-x (- screen-y 5) (- screen-x 8) (+ screen-y 12) color-idx)
              (img-line disp screen-x (- screen-y 5) (+ screen-x 8) (+ screen-y 12) color-idx)
              })
          }
          nil) ;; Don't draw if off-screen
      })
    }))

;; Find ground level at world x position
;;(defun find-ground-y (world-x)
;;  (let ((ground-y (+ 280 (* 20 (sin (* world-x 0.01)))))) ;; Generated ground height
;;    (- ground-y 12))) ;; Position stick figures 12 pixels above ground

(defun in-segment (x seg)
  (if (and (>= x (ix seg 0))
           (<= x (ix seg 2)))
      t
      nil))

(defun find (pred segs)
  (car (filter pred segs)))

(defun interpolate-y (x x1 y1 x2 y2)
  (+ y1 (* (/ (- x x1) (- x2 x1)) (- y2 y1))))

(defun find-ground-y (world-x)
  (let ((pred (lambda (seg) (in-segment world-x seg)))
        (seg (find pred environment)))
    (if seg
        (- (interpolate-y world-x (ix seg 0) (ix seg 1) (ix seg 2) (ix seg 3)) 12)
        0)
    ))
  

;; Stick figure task behavior (always running rightward)
(defun stick-figure-task (initial-x task-id color-idx)
  (let ((x initial-x)
        (direction 1)      ;; Always rightward
        (state 'running)
        (phase 0)
        (speed 1)
        )
    (loopwhile demo-running
      {
      (let ((ground-y (find-ground-y x)))
        {
        (setq x (+ x speed))
        
        ;; Update animation phase
        (setq phase (+ phase 1))
        
        ;; Update stick figure in global list
        (let ((fig-index (- task-id 1)))
          (if (< fig-index (length stick-figures))
              (setix stick-figures fig-index 
                     (list x ground-y direction state task-id color-idx phase))
              nil))
        
        (yield 10000)
        })
      })))

;; Find the leading (rightmost) stick figure for camera tracking
(defun find-leading-runner ()
  (let ((leader-x 0))
    {
    (map (lambda (fig) 
           (let ((fig-x (ix fig 0)))
             (if (> fig-x leader-x)
                 (setq leader-x fig-x)
                 nil)))
         stick-figures)
    leader-x
    }))

;; Update camera to follow leading runner with smooth movement (centered)
(defun update-camera ()
  {
  (let ((leader-x (find-leading-runner)))
    {
    ;; Set camera target to center the leading runner (screen center is at x=200)
    (let ((new-camera-target (- leader-x 200))) ;; Center leader in 400px wide window
      {
      ;; Only move camera forward (rightward), never backward
      (if (> new-camera-target camera-target)
          (setq camera-target new-camera-target)
          nil)
      
      ;; Smooth camera movement (lerp) - always forward
      (let ((camera-speed 0.05)) ;; Responsive camera movement
        (if (> camera-target camera-x)
            (setq camera-x (+ camera-x (* camera-speed (- camera-target camera-x))))
            nil))
      })
    })
  })

;; Create initial stick figures in world coordinates (all running rightward)
(defun spawn-stick-figures ()
  {
  (setq stick-figures 
        '((50 280 1 running 1 2 0)     
          (100 280 1 running 2 3 10)   
          (150 280 1 running 3 4 20)   
          (200 280 1 running 4 5 30)   
          (250 280 1 running 5 6 40))) 
  
  ;; Spawn a task for each stick figure
  (spawn 100 stick-figure-task 50 1 2)
  (spawn 100 stick-figure-task 100 2 3) 
  (spawn 100 stick-figure-task 150 3 4)
  (spawn 100 stick-figure-task 200 4 5)
  (spawn 100 stick-figure-task 250 5 6)
  })

(defun time-waster (i) 
  (loopwhile demo-running {
        (print "time-waster " i )
        (looprange i 0 10000
              (+ 1 2) ;; Wasting time
              )
        }))
             

(defun spawn-timewasters ()
  {
  (spawn 100 time-waster 0)
  (spawn 100 time-waster 1)
  })


;; Task scheduling visualization
(defun draw-task-info ()
  (let ((leader-x (find-leading-runner)))
    {
    (let ((visible-count 0))
      {
      (map (lambda (fig)
             (let ((screen-x (- (ix fig 0) camera-x)))
               (if (and (> screen-x -50) (< screen-x 450))
                   (setq visible-count (+ visible-count 1))
                   nil)))
           stick-figures)
      
      (ttf-text disp 350 25 aa-text ttf (to-str visible-count))
      })
    (ttf-text disp 10 25 aa-text ttf (to-str num-overflows))
    }))

;; Color palette for indexed16 format (16 colors: indices 0-15)
(define color-palette '(0x000000   ;; 0: Black (background)
                       0xffffff   ;; 1: White (environment lines)
                       0xff0000   ;; 2: Red (stick figure 1)
                       0x00ff00   ;; 3: Green (stick figure 2)
                       0x0000ff   ;; 4: Blue (stick figure 3)
                       0xffff00   ;; 5: Yellow (stick figure 4)
                       0xff00ff   ;; 6: Magenta (stick figure 5)
                       0x00ffff   ;; 7: Cyan
                       0x808080   ;; 8: Gray
                       0xff8000   ;; 9: Orange
                       0x8000ff   ;; 10: Purple
                       0x008000   ;; 11: Dark green
                       0x800000   ;; 12: Dark red
                       0x000080   ;; 13: Dark blue
                       0x808000   ;; 14: Olive
                       0x800080)) ;; 15: Maroon

(define num-overflows 0)

(define main-loop
    (lambda ()
      {
      (spawn-stick-figures)
      (spawn-timewasters)
      (var systime-last (systime))
      (loopwhile demo-running {
            (var systime-now (systime))
            (if (> systime-last systime-now) (setq num-overflows (+ 1 num-overflows)))
            (setq systime-last systime-now)
            (update-camera)          ;; Update camera to follow leading runner
            (draw-environment)       ;; Draw scrolling environment
            (map draw-stick-figure stick-figures) ;; Draw all stick figures
            (draw-task-info)
            (disp-render disp 0 0 color-palette)
            (sdl-present rend)
            (yield 33000) ;; ~30 FPS
       })
      }))

(spawn main-loop)
                    
        
 
               
                      
                       
