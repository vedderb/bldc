
(define test_room_persistant_assoc '())

;; Create the room tile map with variety: 0=floor, 1=wall, 2=hieroglyph, 3=door
(define room-tiles [ 1 2 1 1 1 1 2 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 1 1 1 1 1 1 1 ])


;; room thread 
(lambda ()
  {
    ;; Get display buffer from game state
    (var disp (assoc game-state 'disp))

    (print "This is a test room")
    (loopwhile t {
          
          (img-clear disp ) 
          ;; Render the room using the consolidated function
          (render-room-from-tiles disp room-tiles)
          
          ;; Demo: test horizontal snake (using tile coordinates)
          (render-snake-from-path disp 1 1 '())
          (render-snake-from-path disp 1 3 '(we wn ))
          (render-snake-from-path disp 4 3 '(we ws ns ns))
          (disp-render disp 0 0 (list))
          
          ;; Handle messages
          (recv-to 0.1  ; Wait 10ms for messages
                   (look
                    {
                    (print "You are in the test room")
                    })
                   
                   ((look _ ) 
                    (print "The walls are covered in ancient texts written in an obscure language.\n"))
                                    
                   ((go north )
                    {
                    (if (assoc test_room_persistant_assoc 'door-open)
                        (print sender '(room-change 0 . 1))
                        (print sender "The door is sealed shut."))
                    })
                   
                   (quit break)  
                   (no-more break)
                   
                   (timeout ())
                   ((? x) (print x)))  ; Timeout - continue loop
          })
    })
