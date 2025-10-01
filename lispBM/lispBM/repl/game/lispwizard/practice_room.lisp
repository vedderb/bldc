
(define practice-room-persistant-assoc
    (acons 'player-y 3
    (acons 'player-x 4
    (acons 'wizard-y 2
    (acons 'wizard-x 2
    (acons 'cleared t
    (acons 'door-open nil
               '())))))))

(define room-tiles [ 1 2 1 2 2 1 2 1
                     2 0 0 0 0 0 0 2
                     1 0 0 0 0 0 0 1
                     11 0 0 0 0 0 0 7
                     12 0 0 0 0 0 0 8
                     1 0 0 0 0 0 0 1
                     2 0 0 0 0 0 0 2
                     1 2 1 2 2 1 2 1 ])

(define practice-room-done nil)

;; room thread
(lambda ()
  {
  ;; Get display buffer from game state
  (var disp (assoc game-state 'disp))

  (print "The wizard speaks")
  (print "\"This room holds the runes of offense and defense")
  (print "")
 
  (loopwhile (not practice-room-done)
   {
   
   (if (not (assoc practice-room-persistant-assoc 'cleared))
       () ;; room clear logic
       )
 
   (img-clear disp)
   (render-room-from-tiles disp room-tiles)

   ;(render-evil-snake-wielder disp 250 250)
   (render-wizard disp
                  (assoc practice-room-persistant-assoc 'wizard-x)
                  (assoc practice-room-persistant-assoc 'wizard-y))
   (render-player disp
                  (assoc practice-room-persistant-assoc 'player-x)
                  (assoc practice-room-persistant-assoc 'player-y))

   (disp-render disp 0 0 (list))

   ;; Handle messages
   (recv-to 0.1  ; Wait 10ms for messages
            ((look wizard) {
             (setq looked-wizard t)
             (print "The wizard is old and wise.\n")
             })
            ((look door)
             (print "There is a door leading east.\n"))
            ((look runes) {
             (print "There are three groups of runes organised under the titles:")
             (print "movement-runes, attack-runes, defense-runes")
             })
            ((look movement-runes)
             (print "To be decided"))
            ((look attack-runes)
             (print "To be decided"))
            ((look defense-runes)
             (print "To be decided"))
            ((look _) {
             (print "There is a wizard in the room and a door leading east.")
             (print "Strange runes are covering the walls.")
             })
            
            ((go east)
             (if (not (assoc practice-room-persistant-assoc 'door-open))
                 (print "The door is sealed shut.")
                 {
                 (send  (assoc game-state 'main-cid) '(room-change east))
                 (setq practice-room-done t)
                 }
                 )
             )
            ((go _)
             (print "There is no passage in that direction!"))

            ((open door)
             {
             (if (assoc practice-room-persistant-assoc 'cleared)
                 {
                 (open-door room-tiles 7 3)
                 (open-door room-tiles 7 4)
                 (setassoc practice-room-persistant-assoc 'door-open t)
                 (print "The door opens with a grinding sound of ancient stone.")
                 }
                 (print "Impossible! The door is locked!"))
             })
            ((open (? x))
             (print "The " x " cannot be opened.")
             )
            
            (quit break)    ; Add quit message handler
            (no-more break)
            
            (timeout ())
            ((? x) (print x)))  ; Timeout - continue loop
   })
  (print "Leaving the practice room.")
  })
