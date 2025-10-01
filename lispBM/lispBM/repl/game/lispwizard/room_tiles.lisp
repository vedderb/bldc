;; Room Tile System
;; 8x8 grid = 64 bytes, each tile is 50x50 pixels (400x400 total)
;; Tile types: 0=empty, 1=wall, 2=wall+hieroglyph, 3=door, 4=open_door,
;; 5=door_left, 6=door_right, 7=door_top, 8=door_bottom,
;; 9=open_door_left, 10=open_door_right, 11=open_door_top, 12=open_door_bottom,
;; 13=closed_chest, 14=open_chest

;; Tile rendering functions
(define render-tile (lambda (img tile-x tile-y tile-type)
  {
    (var pixel-x (* tile-x 50))
    (var pixel-y (* tile-y 50))
    
    (cond 
      ((eq tile-type 0) 
        {
          ;; Empty space - dark stone floor
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Add subtle floor texture
          (img-rectangle img (+ pixel-x 10) (+ pixel-y 10) 4 4 0x303030)
          (img-rectangle img (+ pixel-x 35) (+ pixel-y 35) 4 4 0x303030)
        })
        
      ((eq tile-type 1)
        {
          ;; Wall - light gray stone
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Add texture lines
          (img-line img pixel-x (+ pixel-y 12) (+ pixel-x 49) (+ pixel-y 12) 0x606060)
          (img-line img pixel-x (+ pixel-y 25) (+ pixel-x 49) (+ pixel-y 25) 0x606060)
          (img-line img pixel-x (+ pixel-y 37) (+ pixel-x 49) (+ pixel-y 37) 0x606060)
          ;; Vertical texture
          (img-line img (+ pixel-x 15) pixel-y (+ pixel-x 15) (+ pixel-y 49) 0x606060)
          (img-line img (+ pixel-x 35) pixel-y (+ pixel-x 35) (+ pixel-y 49) 0x606060)
          ;; Add right and bottom border lines
          (img-line img (+ pixel-x 49) pixel-y (+ pixel-x 49) (+ pixel-y 49) 0x606060)
          (img-line img pixel-x (+ pixel-y 49) (+ pixel-x 49) (+ pixel-y 49) 0x606060)
        })
        
      ((eq tile-type 2)
        {
          ;; Wall with hieroglyph
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Add texture
          (img-line img pixel-x (+ pixel-y 12) (+ pixel-x 49) (+ pixel-y 12) 0x606060)
          (img-line img pixel-x (+ pixel-y 37) (+ pixel-x 49) (+ pixel-y 37) 0x606060)
          ;; Add right and bottom border lines
          (img-line img (+ pixel-x 49) pixel-y (+ pixel-x 49) (+ pixel-y 49) 0x606060)
          (img-line img pixel-x (+ pixel-y 49) (+ pixel-x 49) (+ pixel-y 49) 0x606060)
          ;; Add hieroglyph - larger cross pattern
          (img-rectangle img (+ pixel-x 20) (+ pixel-y 8) 10 34 0xFF4000)
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 20) 34 10 0xFF4000)
          ;; Add dots for decoration
          (img-rectangle img (+ pixel-x 12) (+ pixel-y 12) 6 6 0xFF4000)
          (img-rectangle img (+ pixel-x 32) (+ pixel-y 32) 6 6 0xFF4000)
        })
        
      ((eq tile-type 3)
        {
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Large golden seal in center
          (img-rectangle img (+ pixel-x 12) (+ pixel-y 12) 26 26 0xFFFF00)
          (img-rectangle img (+ pixel-x 18) (+ pixel-y 18) 14 14 0xFF4000)
          ;; Inner detail
          (img-rectangle img (+ pixel-x 22) (+ pixel-y 22) 6 6 0xFFFF00)
        })
        
      ((eq tile-type 4)
        {
          ;; Open door - dark opening with archway
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Stone archway frame
          (img-rectangle img pixel-x pixel-y 50 8 0x808080)
          (img-rectangle img pixel-x (+ pixel-y 42) 50 8 0x808080)
          (img-rectangle img pixel-x pixel-y 8 50 0x808080)
          (img-rectangle img (+ pixel-x 42) pixel-y 8 50 0x808080)
          ;; Archway details
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 8) 4 4 0x606060)
          (img-rectangle img (+ pixel-x 38) (+ pixel-y 8) 4 4 0x606060)
        })
        
      ((eq tile-type 5)
        {
          ;; Left part of horizontal double door - closed
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Left half of golden seal
          (img-rectangle img (+ pixel-x 18) (+ pixel-y 12) 20 26 0xFFFF00)
          (img-rectangle img (+ pixel-x 24) (+ pixel-y 18) 14 14 0xFF4000)
          ;; Left border detail
          (img-rectangle img pixel-x pixel-y 8 50 0x606060)
        })
        
      ((eq tile-type 6)
        {
          ;; Right part of horizontal double door - closed
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Right half of golden seal
          (img-rectangle img (+ pixel-x 12) (+ pixel-y 12) 20 26 0xFFFF00)
          (img-rectangle img (+ pixel-x 12) (+ pixel-y 18) 14 14 0xFF4000)
          ;; Right border detail
          (img-rectangle img (+ pixel-x 42) pixel-y 8 50 0x606060)
        })
        
      ((eq tile-type 7)
        {
          ;; Top part of vertical double door - closed
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Top half of golden seal
          (img-rectangle img (+ pixel-x 12) (+ pixel-y 18) 26 20 0xFFFF00)
          (img-rectangle img (+ pixel-x 18) (+ pixel-y 24) 14 14 0xFF4000)
          ;; Top border detail
          (img-rectangle img pixel-x pixel-y 50 8 0x606060)
        })
        
      ((eq tile-type 8)
        {
          ;; Bottom part of vertical double door - closed
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Bottom half of golden seal
          (img-rectangle img (+ pixel-x 12) (+ pixel-y 12) 26 20 0xFFFF00)
          (img-rectangle img (+ pixel-x 18) (+ pixel-y 12) 14 14 0xFF4000)
          ;; Bottom border detail
          (img-rectangle img pixel-x (+ pixel-y 42) 50 8 0x606060)
        })
        
      ((eq tile-type 9)
        {
          ;; Left part of horizontal double door - open
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Left archway frame
          (img-rectangle img pixel-x pixel-y 50 8 0x808080)
          (img-rectangle img pixel-x (+ pixel-y 42) 50 8 0x808080)
          (img-rectangle img pixel-x pixel-y 8 50 0x808080)
          ;; Left archway detail
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 8) 4 4 0x606060)
        })
        
      ((eq tile-type 10)
        {
          ;; Right part of horizontal double door - open
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Right archway frame
          (img-rectangle img pixel-x pixel-y 50 8 0x808080)
          (img-rectangle img pixel-x (+ pixel-y 42) 50 8 0x808080)
          (img-rectangle img (+ pixel-x 42) pixel-y 8 50 0x808080)
          ;; Right archway detail
          (img-rectangle img (+ pixel-x 38) (+ pixel-y 8) 4 4 0x606060)
        })
        
      ((eq tile-type 11)
        {
          ;; Top part of vertical double door - open
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Top archway frame
          (img-rectangle img pixel-x pixel-y 50 8 0x808080)
          (img-rectangle img pixel-x pixel-y 8 50 0x808080)
          (img-rectangle img (+ pixel-x 42) pixel-y 8 50 0x808080)
          ;; Top archway detail
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 8) 4 4 0x606060)
        })
        
      ((eq tile-type 12)
        {
          ;; Bottom part of vertical double door - open
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Bottom archway frame
          (img-rectangle img pixel-x (+ pixel-y 42) 50 8 0x808080)
          (img-rectangle img pixel-x pixel-y 8 50 0x808080)
          (img-rectangle img (+ pixel-x 42) pixel-y 8 50 0x808080)
          ;; Bottom archway detail
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 38) 4 4 0x606060)
        })
        
      ((eq tile-type 13)
        {
          ;; Closed chest - dark stone floor background
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Chest body - brown wood
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 20) 34 20 0x8B4513)
          ;; Chest lid - darker brown
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 15) 34 8 0x654321)
          ;; Metal bands - dark gray
          (img-rectangle img (+ pixel-x 6) (+ pixel-y 18) 38 3 0x404040)
          (img-rectangle img (+ pixel-x 6) (+ pixel-y 35) 38 3 0x404040)
          ;; Lock - golden
          (img-rectangle img (+ pixel-x 22) (+ pixel-y 22) 6 8 0xFFD700)
          ;; Lock detail
          (img-rectangle img (+ pixel-x 24) (+ pixel-y 24) 2 4 0x000000)
        })
        
      ((eq tile-type 14)
        {
          ;; Open chest - dark stone floor background
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Chest body - brown wood
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 25) 34 15 0x8B4513)
          ;; Open lid - tilted back, darker brown
          (img-rectangle img (+ pixel-x 10) (+ pixel-y 8) 30 12 0x654321)
          ;; Lid hinge shadow
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 20) 34 3 0x404040)
          ;; Metal bands on body
          (img-rectangle img (+ pixel-x 6) (+ pixel-y 35) 38 3 0x404040)
          ;; Interior - dark with golden glow
          (img-rectangle img (+ pixel-x 10) (+ pixel-y 27) 30 10 0x1A1A1A)
          ;; Golden treasure glow
          (img-rectangle img (+ pixel-x 20) (+ pixel-y 30) 10 4 0xFFD700)
          (img-rectangle img (+ pixel-x 22) (+ pixel-y 28) 6 2 0xFFF8DC)
        })
        
)
  }))

;; Render entire room from tile array (8x8) with background and characters
(define render-room-from-tiles (lambda (img tile-array)
  {
    (var tile-index 0)
    (looprange y 0 8 {
        (looprange x 0 8{
            (var tile-type (bufget-u8 tile-array tile-index))
            (render-tile img x y tile-type)
            (setq x (+ x 1))
            (setq tile-index (+ tile-index 1))
          })
        (setq y (+ y 1))
      })
  }))

;; Helper function to set a tile in the byte array
(define set-tile (lambda (tile-array x y tile-type)
  (bufset-u8 tile-array (+ (* y 8) x) tile-type)))

;; Helper function to get a tile from the byte array  
(define get-tile (lambda (tile-array x y)
  (bufget-u8 tile-array (+ (* y 8) x))))

;; Helper function to open a door at specific position
(define open-door (lambda (tile-array x y)
  {
    (var current-tile (bufget-u8 tile-array (+ (* y 8) x)))
    (var new-tile (match current-tile
                         (3 4)    ; door -> open_door
                         (5 9)    ; door_left -> open_door_left
                         (6 10)   ; door_right -> open_door_right
                         (7 11)   ; door_top -> open_door_top
                         (8 12)   ; door_bottom -> open_door_bottom
                         (_ current-tile))) ; no change for non-door tiles
    (bufset-u8 tile-array (+ (* y 8) x) new-tile)
  }))

;; Character rendering functions
;; Legacy pixel-coordinate versions (deprecated - use tile versions instead)
(define render-wizard-pixels (lambda (img x y)
  {
    ;; Wizard robe - dark blue
    (img-rectangle img (+ x 18) (+ y 15) 14 30 0x0000FF)
    ;; Wizard hat - pointed hat
    (img-rectangle img (+ x 22) (+ y 8) 6 12 0x0000FF)
    (img-rectangle img (+ x 24) (+ y 5) 2 8 0x0000FF)
    ;; Face - light skin
    (img-rectangle img (+ x 22) (+ y 12) 6 6 0xFFCCBB)
    ;; Staff - brown with crystal
    (img-rectangle img (+ x 12) (+ y 10) 3 25 0x8B4513)
    (img-rectangle img (+ x 10) (+ y 8) 7 4 0x00FFFF)
  }))

;; Tile-coordinate version (preferred)
(define render-wizard (lambda (img tile-x tile-y)
  (render-wizard-pixels img (* tile-x 50) (* tile-y 50))))

;; Legacy pixel-coordinate version (deprecated - use tile version instead)
(define render-player-pixels (lambda (img x y)
  {
    ;; Player body - green tunic
    (img-rectangle img (+ x 20) (+ y 18) 10 22 0x00AA00)
    ;; Head - light skin
    (img-rectangle img (+ x 22) (+ y 12) 6 6 0xFFCCBB)
    ;; Hair - brown
    (img-rectangle img (+ x 21) (+ y 10) 8 4 0x8B4513)
    ;; Arms - skin colored
    (img-rectangle img (+ x 16) (+ y 20) 4 12 0xFFCCBB)
    (img-rectangle img (+ x 30) (+ y 20) 4 12 0xFFCCBB)
    ;; Legs - brown pants
    (img-rectangle img (+ x 20) (+ y 40) 4 8 0x654321)
    (img-rectangle img (+ x 26) (+ y 40) 4 8 0x654321)
  }))

;; Tile-coordinate version (preferred)
(define render-player (lambda (img tile-x tile-y)
  (render-player-pixels img (* tile-x 50) (* tile-y 50))))

;; Legacy pixel-coordinate version (deprecated - use tile version instead)
(define render-evil-snake-wielder-pixels (lambda (img x y)
  {
    ;; Corrupted wizard - dark robes with red accents
    (img-rectangle img (+ x 18) (+ y 15) 14 30 0x2B2B2B)  ; Dark gray robe
    ;; Evil red trim on robe
    (img-rectangle img (+ x 17) (+ y 15) 16 2 0xFF0000)
    (img-rectangle img (+ x 17) (+ y 43) 16 2 0xFF0000)
    ;; Twisted pointed hat - dark with red tip
    (img-rectangle img (+ x 22) (+ y 8) 6 12 0x2B2B2B)
    (img-rectangle img (+ x 24) (+ y 5) 2 8 0x2B2B2B)
    (img-rectangle img (+ x 24) (+ y 5) 2 3 0xFF0000)  ; Red tip
    ;; Pale corrupted face
    (img-rectangle img (+ x 22) (+ y 12) 6 6 0xDDDDDD)
    ;; Red glowing eyes
    (img-rectangle img (+ x 23) (+ y 13) 2 2 0xFF0000)
    (img-rectangle img (+ x 27) (+ y 13) 2 2 0xFF0000)
    ;; Snake staff - twisted with serpent head
    (img-rectangle img (+ x 10) (+ y 10) 3 25 0x4A4A4A)  ; Dark staff
    ;; Serpent head on staff
    (img-rectangle img (+ x 8) (+ y 8) 7 6 0x006600)    ; Green snake head
    (img-rectangle img (+ x 7) (+ y 9) 2 2 0xFF0000)    ; Red eyes
    (img-rectangle img (+ x 6) (+ y 10) 2 1 0xFF6600)   ; Forked tongue
    ;; Dark aura around feet
    (img-rectangle img (+ x 16) (+ y 44) 18 4 0x1A1A1A)
  }))

;; Tile-coordinate version (preferred)
(define render-evil-snake-wielder (lambda (img tile-x tile-y)
  (render-evil-snake-wielder-pixels img (* tile-x 50) (* tile-y 50))))

;; Snake sprite system for representing lists
;; Snake colors: head=red, body=green, different shades for depth

;; Snake head with triangular snout for better direction indication
(define render-snake-head
    (lambda (img x y direction)
      (cond 
        ((eq direction 'east)
         (progn
           ;; Main head body (rectangular)
           (img-rectangle img (+ x 17) (+ y 18) 18 14 0xFF0000)
           ;; Eyes on the head body
           (img-rectangle img (+ x 29) (+ y 21) 3 3 0xFFFFFF)
           (img-rectangle img (+ x 29) (+ y 27) 3 3 0xFFFFFF)
           ;; Triangular snout pointing west
           (img-rectangle img (+ x 13) (+ y 22) 4 6 0xCC0000)  ; base of triangle
           (img-rectangle img (+ x 11) (+ y 23) 2 4 0xCC0000)  ; middle of triangle  
           (img-rectangle img (+ x 10) (+ y 24) 1 2 0xCC0000)  ; tip of triangle
           ))
        ((eq direction 'west)
         (progn
           ;; Main head body (rectangular)
           (img-rectangle img (+ x 15) (+ y 18) 18 14 0xFF0000)
           ;; Eyes on the head body
           (img-rectangle img (+ x 18) (+ y 21) 3 3 0xFFFFFF)
           (img-rectangle img (+ x 18) (+ y 27) 3 3 0xFFFFFF)
           ;; Triangular snout pointing east (made with rectangles)
           (img-rectangle img (+ x 33) (+ y 22) 4 6 0xCC0000)  ; base of triangle
           (img-rectangle img (+ x 37) (+ y 23) 2 4 0xCC0000)  ; middle of triangle  
           (img-rectangle img (+ x 39) (+ y 24) 1 2 0xCC0000)  ; tip of triangle
           ))
        ((eq direction 'north)
         (progn
           ;; Main head body (rectangular)
           (img-rectangle img (+ x 18) (+ y 17) 14 18 0xFF0000)
           ;; Eyes on the head body
           (img-rectangle img (+ x 21) (+ y 29) 3 3 0xFFFFFF)
           (img-rectangle img (+ x 27) (+ y 29) 3 3 0xFFFFFF)
           ;; Triangular snout pointing north
           (img-rectangle img (+ x 22) (+ y 13) 6 4 0xCC0000)  ; base of triangle
           (img-rectangle img (+ x 23) (+ y 11) 4 2 0xCC0000)  ; middle of triangle  
           (img-rectangle img (+ x 24) (+ y 10) 2 1 0xCC0000)  ; tip of triangle
           ))
        ((eq direction 'south)
         (progn
           ;; Main head body (rectangular)  
           (img-rectangle img (+ x 18) (+ y 15) 14 18 0xFF0000)
           ;; Eyes on the head body
           (img-rectangle img (+ x 21) (+ y 18) 3 3 0xFFFFFF)
           (img-rectangle img (+ x 27) (+ y 18) 3 3 0xFFFFFF)
           ;; Triangular snout pointing south
           (img-rectangle img (+ x 22) (+ y 33) 6 4 0xCC0000)  ; base of triangle
           (img-rectangle img (+ x 23) (+ y 37) 4 2 0xCC0000)  ; middle of triangle  
           (img-rectangle img (+ x 24) (+ y 39) 2 1 0xCC0000)  ; tip of triangle
           ))
        (t (img-rectangle img (+ x 20) (+ y 20) 10 10 0xFF0000)))))

;; Snake body segment - horizontal
(define render-snake-body-h (lambda (img x y)
  (img-rectangle img (+ x 10) (+ y 20) 30 10 0x00AA00)))

;; Snake body segment - vertical
(define render-snake-body-v (lambda (img x y)
  (img-rectangle img (+ x 20) (+ y 10) 10 30 0x00AA00)))

;; Snake corner pieces - 4 basic shapes cover all 8 transitions
;; NE corner (north-east turn and east-north turn)
(define render-snake-corner-ne (lambda (img x y)
  (progn
    ;; Vertical part (north connection)
    (img-rectangle img (+ x 20) (+ y 10) 10 20 0x00AA00)
    ;; Horizontal part (east connection)  
    (img-rectangle img (+ x 25) (+ y 20) 15 10 0x00AA00))))

;; NW corner (north-west turn and west-north turn) 
(define render-snake-corner-nw (lambda (img x y)
  (progn
    ;; Vertical part (north connection)
    (img-rectangle img (+ x 20) (+ y 10) 10 20 0x00AA00)
    ;; Horizontal part (west connection)
    (img-rectangle img (+ x 10) (+ y 20) 15 10 0x00AA00))))

;; SE corner (south-east turn and east-south turn)
(define render-snake-corner-se (lambda (img x y)
  (progn
    ;; Vertical part (south connection)
    (img-rectangle img (+ x 20) (+ y 20) 10 20 0x00AA00)
    ;; Horizontal part (east connection)
    (img-rectangle img (+ x 25) (+ y 20) 15 10 0x00AA00))))

;; SW corner (south-west turn and west-south turn)
(define render-snake-corner-sw (lambda (img x y)
  (progn
    ;; Vertical part (south connection)
    (img-rectangle img (+ x 20) (+ y 20) 10 20 0x00AA00)
    ;; Horizontal part (west connection)
    (img-rectangle img (+ x 10) (+ y 20) 15 10 0x00AA00))))


(define render-snake-tail (lambda (img x y)
  (img-rectangle img (+ x 20) (+ y 20) 10 10 0x006600)))

(define translate-snake-render-pos
    (lambda (x y direction)
      (match direction
             ;; Straight segments
             (ew (cons (- x 50) y))
             (we (cons (+ x 50) y))
             (ns (cons x (+ y 50)))
             (sn (cons x (- y 50)))
             ;; Corner transitions - position depends on turn direction
             (ne (cons (+ x 50) y))
             (en (cons x (- y 50)))
             (nw (cons x (+ y 50)))
             (wn (cons x (- y 50)))
             (se (cons (+ x 50) y))
             (sw (cons (- x 50) y))
             (ws (cons x (+ y 50))))))

(define is-snake
    (let ((is-snake-part
           (lambda (x)
             (match x
                    (ew t)
                    (we t) 
                    (ns t)
                    (sn t)
                    (ne t)  ; north->east turn, head faces south
                    (en t)   ; east->north turn, head faces west  
                    (nw t)  ; north->west turn, head faces south
                    (wn t)   ; west->north turn, head faces east
                    (se t)  ; south->east turn, head faces north
                    (es t)   ; east->south turn, head faces west
                    (sw t)  ; south->west turn, head faces north  
                    (ws t)))))              
      (lambda (ls)
        (cond ((eq ls nil) t)
              ((eq (type-of ls) type-list)
               (if (eq ls nil) t
                   (and (is-snake-part (car ls)) (is-snake (cdr ls)))))))))
                              

(define is-prefix
    (lambda (a b)
      (if (eq a nil) t
          (if (eq (car a) (car b))
              (is-prefix (cdr a) (cdr b))
              nil))))

(define is-suffix
    (lambda (a b)
      (is-prefix (reverse a) (reverse b))))


;; Legacy pixel-coordinate version (deprecated - use tile version instead)
(define render-snake-from-path-pixels (lambda (img head-x head-y directions)
  (if (eq directions nil)
    ;; Empty list - render tombstone (defeated snake) centered in 50x50 tile
    (progn
      ;; Tombstone base (dark gray stone) - centered in tile
      (img-rectangle img (+ head-x 15) (+ head-y 20) 20 25 0x606060 '(filled))
      ;; Tombstone top (rounded with smaller rectangle)
      (img-arc img (+ head-x 25) (+ head-y 20) 10 180 360 0x606060 '(filled))
    )
    ;; Non-empty list - render snake from directions
    (progn
      ;; Draw head facing opposite of first direction
      (var first-dir (car directions))
      (var head-facing (match first-dir 
                              (ew 'west)
                              (we 'east) 
                              (ns 'north)
                              (sn 'south)
                              ;; Corner transitions - head faces the "from" direction
                              (ne 'south)  ; north->east turn, head faces south
                              (en 'west)   ; east->north turn, head faces west  
                              (nw 'south)  ; north->west turn, head faces south
                              (wn 'east)   ; west->north turn, head faces east
                              (se 'north)  ; south->east turn, head faces north
                              (es 'west)   ; east->south turn, head faces west
                              (sw 'north)  ; south->west turn, head faces north  
                              (ws 'east)   ; west->south turn, head faces east
                              (_ 'east)))
      (render-snake-head img head-x head-y head-facing)
      ;; Draw body segments
      
      (var (curr-x . curr-y) (translate-snake-render-pos head-x head-y first-dir))
      
      (var dir-list (cdr directions))
      (loopwhile dir-list {
            (var dir (car dir-list))
            ;; Move to next position based on direction
            ;; Draw appropriate segment
            ;; Body segment or corner
            (match dir
              ;; Corner pieces
              (ne (render-snake-corner-ne img curr-x curr-y))
              (en (render-snake-corner-ne img curr-x curr-y))
              (nw (render-snake-corner-nw img curr-x curr-y))
              (wn (render-snake-corner-nw img curr-x curr-y))
              (se (render-snake-corner-se img curr-x curr-y))
              (es (render-snake-corner-se img curr-x curr-y))
              (sw (render-snake-corner-sw img curr-x curr-y))
              (ws (render-snake-corner-sw img curr-x curr-y))
              ;; Straight body segments
              (ew (render-snake-body-h img curr-x curr-y))
              (we (render-snake-body-h img curr-x curr-y))
              (ns (render-snake-body-v img curr-x curr-y))
              (sn (render-snake-body-v img curr-x curr-y)))

            (var (new-x . new-y) (translate-snake-render-pos curr-x curr-y dir))
            (setq curr-x new-x)
            (setq curr-y new-y)  
            
            (setq dir-list (cdr dir-list))
      })
      (render-snake-tail img curr-x curr-y)
      )
    )))

;; Tile-coordinate version (preferred)
(define render-snake-from-path (lambda (img tile-x tile-y directions)
  (render-snake-from-path-pixels img (* tile-x 50) (* tile-y 50) directions)))

;; Demo function: render a sample snake representing list '(1 2 3)
(define demo-snake (lambda (img)
  {
    ;; Simple L-shaped snake: head east, corner, body down, tail
    (var snake-path '((50 100 'east 'head)
                      (100 100 'east 'corner-se) 
                      (100 150 'south 'body-v)
                      (100 200 'south 'tail)))
    (render-snake-from-path img snake-path)
    (var a-dead-snake '())
    (render-snake-from-path img a-dead-snake)
  }))

