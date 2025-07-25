;; The guide to efficient LBM programs

(def ch-intro
    (section 2 "Introduction"
	     (list
	      (para (list "This document contains examples of small LBM programs together with"
			  "discussions on the performance and memory usage characteristics thereof."			  
			  ))
	      )
	     )
  )

(define manual
    (list
     (section 1 "The guide to efficient LBM programs"
              (list ch-intro
		    info
                    )
              )
     )
  )



(defun render-manual ()
  (let ((h (fopen "efficient.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "The guide to efficient LBM programs was generated in " (secs-since t0) " seconds")
    }
    )
  )

