
(define entry-seed
  (ref-entry "seed"
             (list
              (para (list "`seed` sets the seed of the random number generator."
                          "The form of a `seed` expression is `(seed expr)` where `expr`"
                          "must evaluate to a number."
                          "Returns `t`."
                          ))
              (code '((seed 42)
                      (seed 177739)
                      ))
              end)))

(define entry-random
  (ref-entry "random"
             (list
              (para (list "`random` generates the next pseudo-random number."
                          "The form of a `random` expression is `(random)`."
                          "Returns an unsigned integer (`type-u`)."
                          "The range of values and the algorithm used are platform-dependent."
                          "In the LispBM reference implementation a linear congruential generator"
                          "is used and values are in the range 0 to 268435182, chosen to fit"
                          "within the 28-bit value range of `type-u` on 32-bit platforms."
                          ))
              (code '((random)
                      (random)
                      (random)
                      (mod (random) 11)
                      ))
              end)))

(define chapter-random
  (section 2 "Random Number Generation"
           (list entry-seed
                 entry-random
                 )))

(define manual
  (list
   (section 1 "LispBM Random Extensions Reference Manual"
            (list
             (para (list "The random extensions provide functions for seeding and"
                         "generating pseudo-random numbers."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             chapter-random
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "randomref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Random extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
