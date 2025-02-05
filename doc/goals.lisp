
(define manual
  (list
   (section 1 "LispBM goals and vision"
            (list
             (para (list "The definition of vision that we use in this document is \"what we aspire to, but may not currently know exactly how to reach\"."
                         "Goals are more concrete and measureable."
                         ))
             (para (list "This document starts at vision, and derives a set of semi-long term attainable goals."
                         "The later part of the document is the most dynamic and this is where"
                         "long term goals is split up into short term tasks or ideas."
                         ))
             (para (list 
                         ))
             (para (list "The purpose of having a goals and vision document is to have an agreed upon and discussed direction to take LBM."
                         "Mainly this has been set up to releave the stress of the \"core team of devs doing the implementation work\" ;)."
                         "The contents of this document is open for discussion and the happiest the \"core team of devs\" will be if a"
                         "an understanding for it's motivations and desires is reached and respected. However, Nothing in this document"
                         "is holy, and good ideas or insighs may change the entire vision. However, it is expected that this document is"
                         "more dynamic the further into it you read."
                         ))
             )
            )
   (section 2 "Vision"
            (list
             (para (list "Our vision is for LispBM to be a scripting language very well suited for microcontrollers an embedded in general"
                         "and, specifically, for VESC devices (motorcontrollers and auxiliery)."
                         "Ideally LispBM should be usable in all areas including safety or security critical applications."
                         ))
             (para (list "We identity the following key properties of a scripting language for our target platforms."
                         ))
             (bullet '("Small memory footprint of the runtime system."
		       "Predictable performance."
		       "Predictable resource usage." 
                       "Sandboxing - scripts can only influence the rest of the application through the specified interface."
		       "Developer ergonomics. Make things you are likely to want to do on an embedded device builtin."
                       "Easy to learn and use."
                       ))
             (para (list "TODO: add more bullets"
                         ))
             ))
   (section 2 "Goals"
            (list
             (para (list "When selecting what to implement/add/change in LBM one should consider it's value according to the following questions:"
                         ))
             (bullet '("How easy is it to implement?"
                       "How much code does it add?"
                       "How fun is it to implement?"
                       "How useful is the addition or change?"
                       "Does it break compatibility with existing programs?"
                       ))
             (section 3 "Long term goals"
                      (list
                       (para (list "**Bootup time improvements:**"
                                   "The big issue with startup-time improvements via \"images\" is how to handle state that needs to be initialized upon startup."
                                   "Handles in the image to resources allocated on the C side, will be stale!"
                                   ))
                       (bullet '("by improving the reader efficiency (very hard given how complex the reader is already with streaming support)."
                                 "by saving an image of the running system and restoring that upon startup (circumventing the reader)."
                                 "by running the reader \"off-line\" and storing a flattened read-result. (This would be superseeded if there is a byte-code compiler in the future.)"
                                 ))                                 
                       (para (list "**Byte code compilation and execution**"
                                   "ranks very high on the how fun is it axis and will be implemented at some point. Byte code evaluator unit may be a compile time opt-in."'
                                   "Byte code could be stored in flash and executed from flash easily and then also help in speeding up startup-times."
                                   ))
                       (para (list "**Syntax checker, linter etc**"
                                   "A tool for analysis of LBM programs and do a lot of the syntax checking that the reader does not do."
                                   ))
                       )
                      )
             ))
   (section 2 "Continuosly ongoing work"
            (list
             (bullet '("Code size optimisations - do more with less (and add space for fun additions)."
                       "Improving upon testing frameworks and tests."
                       "Improving and expanding on documentation."
                     ))
             ))
   (section 2 "Concrete TODOs"
            (list
             (bullet '(" [ ] flattening and unflattening of a defrag-mem region and the data it contains."
                       ))
             )
            )
                     
             
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "goals.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Goals and vision document was generated in " (secs-since t0) " seconds")
    }
    )
  )
