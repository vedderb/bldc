;; The LBM guide to the strange and unexpected

(def ch-intro
    (section 2 "Introduction"
	     (list
	      (para (list "This document collects strange and unexpected behaviors of the"
			  "LispBM runtime system and language."
			  "If you find any strange and unexpected behaviors while exploring LBM, please contact"
			  "bo(dot)joel(dot)svensson(swirly-a)gmail(dot)com with information about how to trigger the strange"
			  "behavior."
			  ))
	      )
	     )
  )


(def ch-constant
    (list 
     (section 2 "Constant code and data"
              (list
               (para (list "As LBM is targeting microcontrollers RAM is a limited resource while FLASH can be quite abundant!"
                           "LBM allows the usage of flash for storage of both code and data and this is a source of a lot of"
                           "interesting and perhaps unexpected behaviors."
                           "Lisps are usually very liberal about what can be mutated. In essense just about everything can be mutated."
                           "Constant storage destroys this property."
                           ))
               )
              )
     (section 3 "Literals are constant in constant blocks"
              (list
               
	       )
	      )
     )
  )
	   

(define manual
    (list
     (section 1 "The LBM guide to the strange and unexpected"
	      (list ch-intro
		    ch-constant
		    info
		    )
	      )
     )
  )



(defun render-manual ()
  (let ((h (fopen "strange.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "The LBM guide to the strange and unexpected was generated in " (secs-since t0) " seconds")
    }
    )
  )

