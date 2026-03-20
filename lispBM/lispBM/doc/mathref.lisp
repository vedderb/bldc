
(define entry-sin
    (ref-entry "sin"
               (list
                (para (list "Computes the sine of a given angle (in radians)."
                            "The form of a `sin` expression is `(sin expr)`."
                            ))
                (code '((sin 0.5)
                        (sin 3.1415926)
                        (sin (* 0.5 3.141592))
                        ))
                end)))

(define entry-cos
    (ref-entry "cos"
               (list
                (para (list "Computes the cosine of a given angle (in radians)."
                            "The form of a `cos` expression is `(cos expr)`."
                            ))
                (code '((cos 0.5)
                        (cos 3.1415926)
                        (cos (* 0.5 3.141592))
                        ))
                end)))

(define entry-tan
    (ref-entry "tan"
               (list
                (para (list "Computes the tangent of a given angle (in radians)."
                            "The form of a `tan` expression is `(tan expr)`."
                            ))
                (code '((tan 0.5)
                        (tan 3.1415926)
                        (tan (* 0.5 3.141592))
                        ))
                end)))

(define entry-asin
    (ref-entry "asin"
               (list
                (para (list "Computes the arcsine of a value, returning an angle in radians."
                            "The form of an `asin` expression is `(asin expr)`."
                            "The argument should be in the range [-1, 1]."
                            ))
                (code '((asin 0.0)
                        (asin 1.0)
                        (asin 0.5)
                        ))
                end)))

(define entry-acos
    (ref-entry "acos"
               (list
                (para (list "Computes the arccosine of a value, returning an angle in radians."
                            "The form of an `acos` expression is `(acos expr)`."
                            "The argument should be in the range [-1, 1]."
                            ))
                (code '((acos 0.0)
                        (acos 1.0)
                        (acos 0.5)
                        ))
                end)))

(define entry-atan
    (ref-entry "atan"
               (list
                (para (list "Computes the arctangent of a value, returning an angle in radians."
                            "The form of an `atan` expression is `(atan expr)`."
                            ))
                (code '((atan 0.0)
                        (atan 1.0)
                        (atan -1.0)
                        ))
                end)))

(define entry-atan2
    (ref-entry "atan2"
               (list
                (para (list "Computes the arctangent of `y/x`, using the signs of both arguments"
                            "to determine the correct quadrant. Returns an angle in radians in the"
                            "range [-pi, pi]. The form of an `atan2` expression is `(atan2 y x)`."
                            ))
                (code '((atan2 1.0 1.0)
                        (atan2 0.0 -1.0)
                        (atan2 -1.0 0.0)
                        ))
                end)))

(define entry-pow
    (ref-entry "pow"
               (list
                (para (list "Raises a base to a given exponent."
                            "The form of a `pow` expression is `(pow base exponent)`."
                            ))
                (code '((pow 2.0 10.0)
                        (pow 3.0 3.0)
                        (pow 2.0 0.5)
                        ))
                end)))

(define entry-exp
    (ref-entry "exp"
               (list
                (para (list "Computes the exponential function e raised to the power of the argument."
                            "The form of an `exp` expression is `(exp expr)`."
                            ))
                (code '((exp 0.0)
                        (exp 1.0)
                        (exp -1.0)
                        ))
                end)))

(define entry-sqrt
    (ref-entry "sqrt"
               (list
                (para (list "Computes the square root of a number."
                            "The form of a `sqrt` expression is `(sqrt expr)`."
                            ))
                (code '((sqrt 4.0)
                        (sqrt 2.0)
                        (sqrt 0.0)
                        ))
                end)))

(define entry-log
    (ref-entry "log"
               (list
                (para (list "Computes the natural logarithm (base e) of a number."
                            "The form of a `log` expression is `(log expr)`."
                            ))
                (code '((log 1.0)
                        (log 2.718281828)
                        (log 10.0)
                        ))
                end)))

(define entry-log10
    (ref-entry "log10"
               (list
                (para (list "Computes the base-10 logarithm of a number."
                            "The form of a `log10` expression is `(log10 expr)`."
                            ))
                (code '((log10 1.0)
                        (log10 10.0)
                        (log10 100.0)
                        ))
                end)))

(define entry-floor
    (ref-entry "floor"
               (list
                (para (list "Rounds a number down to the nearest integer, returning a float."
                            "The form of a `floor` expression is `(floor expr)`."
                            ))
                (code '((floor 1.6)
                        (floor -1.6)
                        (floor 2.0)
                        ))
                end)))

(define entry-ceil
    (ref-entry "ceil"
               (list
                (para (list "Rounds a number up to the nearest integer, returning a float."
                            "The form of a `ceil` expression is `(ceil expr)`."
                            ))
                (code '((ceil 1.2)
                        (ceil -1.2)
                        (ceil 2.0)
                        ))
                end)))

(define entry-round
    (ref-entry "round"
               (list
                (para (list "Rounds a number to the nearest integer, returning a float."
                            "Halfway cases are rounded away from zero."
                            "The form of a `round` expression is `(round expr)`."
                            ))
                (code '((round 1.4)
                        (round 1.5)
                        (round -1.5)
                        ))
                end)))

(define entry-deg2rad
    (ref-entry "deg2rad"
               (list
                (para (list "Converts degrees to radians."
                            "When called with a single argument the form is `(deg2rad expr)`."
                            "When called with multiple arguments, each is converted and the"
                            "results are returned as a list."
                            ))
                (code '((deg2rad 180.0)
                        (deg2rad 90.0)
                        (deg2rad 0.0 90.0 180.0 270.0 360.0)
                        ))
                end)))

(define entry-rad2deg
    (ref-entry "rad2deg"
               (list
                (para (list "Converts radians to degrees."
                            "When called with a single argument the form is `(rad2deg expr)`."
                            "When called with multiple arguments, each is converted and the"
                            "results are returned as a list."
                            ))
                (code '((rad2deg 3.141592)
                        (rad2deg 1.5707963)
                        (rad2deg 0.0 1.5707963 3.141592)
                        ))
                end)))

(define entry-is-nan
    (ref-entry "is-nan"
               (list
                (para (list "`is-nan` checks whether a floating-point value is NaN (not a number)."
                            "Returns `t` if the value is NaN and `nil` otherwise."
                            "The form of an `is-nan` expression is `(is-nan expr)`."
                            "Only `float` and `double` values can be NaN; any other number type returns `nil`."
                            "NaN can arise from operations such as `(sqrt -1.0)` or `(asin 2.0)`."
                            "Note that division by zero is an error in LispBM and does not produce NaN."
                            ))
                (code '((is-nan (sqrt -1.0))
                        (is-nan (asin 2.0))
                        (is-nan 1.0)
                        ))
                end)))

(define entry-is-inf
    (ref-entry "is-inf"
               (list
                (para (list "`is-inf` checks whether a floating-point value is infinite."
                            "Returns `t` if the value is positive or negative infinity, `nil` otherwise."
                            "The form of an `is-inf` expression is `(is-inf expr)`."
                            "Only `float` and `double` values can be infinite; any other number type returns `nil`."
                            "Infinity can arise from operations such as `(exp 1000.0)` or `(log 0.0)`."
                            "Note that division by zero is an error in LispBM and does not produce infinity."
                            ))
                (code '((is-inf (exp 1000.0))
                        (is-inf (log 0.0))
                        (is-inf 1.0)
                        ))
                end)))

(define chapter-trig
  (section 2 "Trigonometry"
           (list entry-sin
                 entry-cos
                 entry-tan
                 entry-asin
                 entry-acos
                 entry-atan
                 entry-atan2
                 )))

(define chapter-explog
  (section 2 "Exponential and Logarithm"
           (list entry-pow
                 entry-exp
                 entry-sqrt
                 entry-log
                 entry-log10
                 )))

(define chapter-rounding
  (section 2 "Rounding"
           (list entry-floor
                 entry-ceil
                 entry-round
                 )))

(define chapter-conversion
  (section 2 "Angle Conversion"
           (list entry-deg2rad
                 entry-rad2deg
                 )))

(define chapter-predicates
  (section 2 "Floating-point Predicates"
           (list entry-is-nan
                 entry-is-inf
                 )))

(define manual
  (list
   (section 1 "LispBM Math Extensions Reference Manual"
            (list
             (para (list "The math extensions provide mathematical functions such as"
                         "trigonometry, logarithms, rounding and related utilities."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             chapter-trig
             chapter-explog
             chapter-rounding
             chapter-conversion
             chapter-predicates
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "mathref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Math extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
