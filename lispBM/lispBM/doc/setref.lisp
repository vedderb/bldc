
(define entry-set-insert
  (ref-entry "set-insert"
             (list
              (para (list "`set-insert` inserts a value into a set."
                          "Sets are represented as lists with no duplicate elements."
                          "The form of a `set-insert` expression is `(set-insert set value)`."
                          "If `value` is already a member of `set` the original set is returned unchanged."
                          "Otherwise a copy of the set is returned with `value` appended."
                          "Membership is tested using structural equality."
                          ))
              (code '((set-insert nil 1)
                      (set-insert (list 1 2 3) 4)
                      (set-insert (list 1 2 3) 2)
                      ))
              end)))

(define entry-set-union
  (ref-entry "set-union"
             (list
              (para (list "`set-union` computes the union of two sets."
                          "Sets are represented as lists with no duplicate elements."
                          "The form of a `set-union` expression is `(set-union set1 set2)`."
                          "Returns a set containing all elements from both `set1` and `set2`"
                          "with no duplicates."
                          "Membership is tested using structural equality."
                          ))
              (code '((set-union (list 1 2 3) (list 4 5 6))
                      (set-union (list 1 2 3) (list 2 3 4))
                      (set-union nil (list 1 2 3))
                      ))
              end)))

(define entry-member
  (ref-entry "member"
             (list
              (para (list "`member` checks if a value is an element of a list."
                          "The form of a `member` expression is `(member value list)`."
                          "Returns `list` if `value` is found anywhere in it, or `nil` if not found."
                          "Equality is tested structurally in the same way as `eq`,"
                          "so number types must match."
                          "Note that `member` is part of core LispBM and is always available."
                          ))
              (code '((member 3 (list 1 2 3))
                      (member 3u (list 1 2 3))
                      (member 'x (list 'a 'b 'x 'y))
                      (member 'z (list 'a 'b 'x 'y))
                      ))
              end)))

(define chapter-sets
  (section 2 "Set Operations"
           (list entry-member
                 entry-set-insert
                 entry-set-union
                 )))

(define manual
  (list
   (section 1 "LispBM Set Extensions Reference Manual"
            (list
             (para (list "The set extensions provide operations for working with sets"
                         "represented as lists of unique elements."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             chapter-sets
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "setref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Set extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
