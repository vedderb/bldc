
(define start-room-persistant-assoc
    (acons 'player-y 3
    (acons 'player-x 4
    (acons 'wizard-y 2
    (acons 'wizard-x 2
    (acons 'cleared nil
    (acons 'door-open nil
    (acons 'chest-open nil              
               '()))))))))

(define room-tiles [ 1 2 1 1 1 1 2 1
                     2 0 0 0 0 0 0 2
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 7
                     1 0 0 0 0 0 0 8
                     1 0 0 0 0 0 0 1
                     2 0 0 0 0 0 13 2
                     1 2 1 1 1 1 2 1 ])

(define start-room-done nil)

;; room thread
(lambda ()
  {
  ;; Get display buffer from game state
  (var disp (assoc game-state 'disp))

  (print "The wizard speaks to you in a thundering voice:")
  (print "\"I have brought you here because the world is in trouble.\"")
  (print "\"The language of the gods is fading from memory.\"")
  (print "\"But you still have the potential to learn the language...\"")
  (print "\"and to save us from the evil that is poisoning the minds of our young.\"")
  (print "")

  (var t0 (systime))
  (var help1-displayed nil)
  (var help2-displayed nil)
  (var looked-wizard nil)
  (var looked-wizard-displayed nil)
  
  (loopwhile (not start-room-done)
   {

   (if (and (not help1-displayed) (< 10 (secs-since t0)))
       {
       (setq help1-displayed t)
       (print "The wizard sighs and says more gently:")
       (print "\"The language of the gods requires precise incantations.\"")
       (print "\"You need to first figure out how to interact with your surroundings.\"")
       (print "\"We can only act in, and on, this magic realm through incantations.\"")
       (print "\"I can teach you the basics...\"")
       (print "")
       })
   
   (if (and (not help2-displayed) (< 20 (secs-since t0)))
       {
       (setq help2-displayed t)
       (print "The wizard continues:")
       (print "\"First, try to perceive more details of your surroundings by looking at things.\"")
       (print "\"Look at me by using the incantation (look wizard)\"")
       (print "")
              
       })

   (if (and looked-wizard (not looked-wizard-displayed))
       {
       (setq looked-wizard-displayed t)
       (print "The wizard smiles:")
       (print "\"Very good. Now try to look at other things.\"")
       (print "")
       }
       )
   
   (if (not (assoc start-room-persistant-assoc 'cleared))
       () ;; room clear logic
       )
   

   (img-clear disp)
   (render-room-from-tiles disp room-tiles)

   ;(render-evil-snake-wielder disp 250 250)
   (render-wizard disp
                  (assoc start-room-persistant-assoc 'wizard-x)
                  (assoc start-room-persistant-assoc 'wizard-y))
   (render-player disp
                  (assoc start-room-persistant-assoc 'player-x)
                  (assoc start-room-persistant-assoc 'player-y))

   (disp-render disp 0 0 (list))

   ;; Handle messages
   (recv-to 0.1  ; Wait 10ms for messages
            ((look wizard) {
             (setq looked-wizard t)
             (print "The wizard is old and wise.\n")
             })
            ((look door)
             (print "There is a door leading east.\n"))
            ((look chest)
             (print "The chest looks old. There does not seem to be any lock on it.\n"))
            ((look runes) {
             (print "The runes read:")
             (print "nil cons cdr lambda eval") 
             })
            ((look _) {
             (print "There is a wizard in the room and a door leading east.")
             (print "Strange runes are covering the walls.")
             (print "There is a chest in the corner of the room.")
             })
            
            ((go east)
             (if (not (assoc start-room-persistant-assoc 'door-open))
                 (print "The door is sealed shut.")
                 {
                 (send  (assoc game-state 'main-cid) '(room-change east))
                 (setq start-room-done t)
                 }
                 )
             )
            ((go _)
             (print "There is no passage in that direction!"))

            ((open chest) {
             (if (not (assoc start-room-persistant-assoc 'chest-open)) {
                 (set-tile room-tiles 6 6 14)
                 (setassoc start-room-persistant-assoc 'chest-open t)
                 (setassoc start-room-persistant-assoc 'cleared t)
                 (print "The chest opens with a creak. Inside you find the key to the door.")
                 (print "")
                 (print "The wizard nods approvingly:")
                 (print "\"The door will open for you now.\"")
                 }
                 (print "The chest is already open"))
             })
            ((open door)
             {
             (if (assoc start-room-persistant-assoc 'cleared)
                 {
                 (open-door room-tiles 7 3)
                 (open-door room-tiles 7 4)
                 (setassoc start-room-persistant-assoc 'door-open t)
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
  (print "Leaving the start room.")
  })
