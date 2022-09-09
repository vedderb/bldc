

(defun f ()
  (progn
    (recv
     ( monkey (print "That's an ape!"))
     ( cat    (print "What a cute cat!"))
     ( (? x)  (print "I dont know kind of animal a " x " is!")))
    (f))) 
  
(spawn f)
