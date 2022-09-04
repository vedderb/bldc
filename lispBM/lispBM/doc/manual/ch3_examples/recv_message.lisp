

(defun f ()
  (progn
    (recv
     ( monkey (print "That's an ape!"))
     ( cat    (print "What a cute cat!"))
     ( _      (print "I dont know what that is!")))
    (f))) 
  
(spawn f)
