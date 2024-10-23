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


(define manual
    (list
     (section 1 "The LBM guide to the strange and unexpected"
              (list ch-intro
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

