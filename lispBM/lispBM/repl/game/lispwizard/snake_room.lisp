
(define ernst-hugo '(sn se we we ws))

(define snake-room-persistant-assoc
    (acons 'player-y 4
    (acons 'player-x 2
    (acons 'wizard-y 3
    (acons 'wizard-x 1
    (acons 'cleared nil
    (acons 'door-open nil
               '())))))))

;; Create the room tile map with variety: 0=floor, 1=wall, 2=hieroglyph, 3=door
(define room-tiles [ 1  2 1 5 6 1 2 1
                     1  0 0 0 0 0 0 1
                     1  0 0 0 0 0 0 1
                     11 0 0 0 0 0 0 1
                     12 0 0 0 0 0 0 1
                     1  0 0 0 0 0 0 1
                     1  0 0 0 0 0 0 1
                     1  1 1 1 1 1 1 1 ])

(define snake-room-done nil)

;; room thread
(lambda ()
  {
  ;; Get display buffer from game state
  (var disp (assoc game-state 'disp))

  (print "The wizard leans towards you and whispers:")
  (print "\"The door is blocked by a serpentine monstrosity.\"")
  (print "\"To battle this beast you must determine its name.\"")
  (print "\"look at the snake (look snake) for clues to its identity.\"")

  (loopwhile (not snake-room-done) {

        (if (not (assoc snake-room-persistant-assoc 'cleared))
            (cond ((eq nil ernst-hugo) {
                   (print "Excellent! You cleared a path to the door.")
                   (setassoc snake-room-persistant-assoc 'cleared t)
                   })
                  ((and (is-snake ernst-hugo) (<= (length ernst-hugo) 1)) {
                   (print "Fantastic! You cleared a path to the door.")
                   (print "Virtously you let the snake live.")
                   (setassoc snake-room-persistant-assoc 'cleared t)
                   })
                  ((or (not (is-snake ernst-hugo))
                       (not (is-suffix ernst-hugo'(sn se we we ws)))) {
                     (setq ernst-hugo '(sn se we we ws))
                     (print "ernst-hugo is resisting your attack!")
                     })
                  )
            )

        (img-clear disp)
        (render-room-from-tiles disp room-tiles)
        (render-snake-from-path disp 2 2 ernst-hugo)

        (render-wizard disp
                       (assoc snake-room-persistant-assoc 'wizard-x)
                       (assoc snake-room-persistant-assoc 'wizard-y))
        (render-player disp
                       (assoc snake-room-persistant-assoc 'player-x)
                       (assoc snake-room-persistant-assoc 'player-y))

        (disp-render disp 0 0 (list))

        ;; Handle messages
        (recv-to 0.1  ; Wait 10ms for messages
                 ((look wizard) {
                  (print "The wizard is old and wise.")
                  (print "")
                  (print "The wizard speaks:")
                  (print "\"You can always look around for clues.\"")
                  })
                 ((look snake) {
                  (print "The snake is quite scary looking.")
                  (print "")
                  (print "The snake hisses:")
                  (print "\"Who dares disturb ernst-hugo?\"")
                  (print "")
                  (print "The wizard exclaims:")
                  (print "\"We are in luck the vicious serpent gives his name freely!\"")
                  (print "")
                  (print "ernst-hugo snaps back:")
                  (print "\"Puny mortals, you are no danger to me.\"")
                  (print "\"I was defined into existence by the ancient gods\"")
                  (print "\"and can only be destroyed by equally powerful magic.\"")
                  (print "")
                  })
                 ((look grave)
                  (if (eq ernst-hugo nil)
                      (print "Here lies ernst-hugo loving father and caring husband.")
                      (Print "What grave?");
                 ((look door)
                  (if (assoc snake-room-persistant-assoc 'door-open)
                      (print "The door towards the north is open.\n")
                      (print "The door towards the north is closed.\n")))
                 ((look _) {
                  (print "You stand in an ancient stone chamber.")
                  (print "A wise wizard watches from the shadows.")
                  (print "To the north, a scaled beast coils before sealed doors.")
                  })

                 ((go north)
                  (if (not (assoc snake-room-persistant-assoc 'door-open))
                      (print "The door is sealed shut.")
                      {
                      ;(print "You leave through the door towards the north")
                      (send  (assoc game-state 'main-cid) '(room-change north))
                      (setq snake-room-done t)
                      }
                      )
                  )
                 ((go west)
                  nil
                  )


                 ((open door)
                  {
                  (if (assoc snake-room-persistant-assoc 'cleared)
                      {
                      (open-door room-tiles 3 0)
                      (open-door room-tiles 4 0)
                      (setassoc snake-room-persistant-assoc 'door-open t)
                      (print "The door opens with a grinding sound of ancient stone.")
                      }
                      (print "Impossible, there is a giant snake in the way"))
                  })

                 (quit break)    ; Add quit message handler
                 (no-more break)

                 (timeout ())
                 ((? x) (print x)))  ; Timeout - continue loop
        })
  (print "Leaving the snake room.")
  })
