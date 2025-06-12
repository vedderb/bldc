
;; Variables in LBM explained for beginners

;; In LBM you create a "global binding" (variable assignment) by use of define or def.
;; define and def are the same thing, def is an alias for define.

(define a 10)
(def b 20)

;; a is now 10 and b 20.
;; define and def creates the association of key and value in the "global" environment
;; which is implemented as a hashtable for efficient lookups.

;; a global binding can be shadowed by a local binding of the same key

(let ((a 30)) {
     (print a) ;; prints 30 not 10.
     (print b) ;; Still prints 20.
  }
  )

;; a global binding can be changed with set, setq or setvar.
;; set and setvar are the same thing. setvar is just an alias for set. 

;; a binding can be updated with set. 
(set 'a 100)

;; note that a is quoted, arguments are normally evaluated and thus
;; (set a 100) would really mean (set 10 100) which is not what we want.
;; by quoting a, 'a we say that we do not want to evaluate a to 10, we want the a itself.

;; But! define and def took an unquoted a and b ? what is up with that?
;; This is possible because define and def are so-called special forms, or part
;; of the LBM syntax, they are not a regular application. Special forms
;; can choose to evaluate or not the arguments freely.

;; setq can also be used to change the value of a binding.

(setq b 200)

;; similarly to define setq does not take a quoted argument.

;; set and setq are both useful. But for a beginner setq is probably more attractive.
;; set will be useful when the trainingwheels are off and we are writing code that
;; is a bit more "meta" (self-referential).


;; Local variables (bindings) created with let are stored as an association list
;; internally and the efficiency of lookup is dependent on the number of local bindings.
;; Being frugal with variables is always good for performance.
;; Global variables have the same problem but it is offset by the hashtable and the number
;; of variables can be pretty big before it takes a performance hit.
;; Now, dont stay away from locals because of that. You often don't need that many.


;; Local variables can be created with let or as progn variables.

(let ((kurt "kurt")
      (russel "russel"))
  (print kurt " " russel))

;; behaves the same as

{
(var kurt "kurt")
(var russel "russel")
(print kurt " " russel)
}
        
;; Note that local variables can also be altered with set and setq.


{
(var kurt "kurt")
(var russel "russel")
(setq kurt "michael")
(setq russel "shanks")
(print kurt " " russel)
}

;; and this works in the let case as well of course
(let ((kurt "kurt")
      (russel "russel")) {
      (setq kurt "michael")
      (setq russel "shanks")
      (print kurt " " russel)
      }
  )

;; let and progn variables are internally represented in the same way and have the same
;; capabilities.


;; LBM support deconstructive bindings. This means that a let or progn var binding
;; can be used to deconstruct datastructures.

;; as an example define a pair
(define my-pair (cons 10 20))

;; my-pair is represented on the heap as a cons-cell with 10 in the first (car) field and
;; 20 in the second (cdr) field.

(let (( (i . j) my-pair)) 
  (print i j) ;; here i is 10 and j is 20. 
  )

;; similarly

{
(var (i . j) my-pair)
(print i)
(print j)
}



