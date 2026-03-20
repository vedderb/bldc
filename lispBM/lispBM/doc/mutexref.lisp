
(define entry-mutex-create
  (ref-entry "mutex-create"
             (list
              (para (list "`mutex-create` creates a mutex for mutual exclusion between"
                          "concurrent LispBM processes."
                          "The form of a `mutex-create` expression is `(mutex-create)`."
                          "Returns a mutex object."
                          "Internally the mutex is represented as a dotted pair used as a"
                          "queue of waiting process IDs, and can be accidentally destroyed"
                          "by standard list operations."
                          ))
              (code '((define m (mutex-create))
                      ))
              end)))

(define entry-mutex-lock
  (ref-entry "mutex-lock"
             (list
              (para (list "`mutex-lock` acquires a mutex."
                          "The form of a `mutex-lock` expression is `(mutex-lock mutex)`."
                          "If the mutex is unlocked, the calling process acquires it and returns `t`."
                          "If the mutex is already held by another process, the calling process"
                          "is blocked until the mutex becomes available."
                          "A process must not attempt to lock a mutex it already holds,"
                          "as this will cause a deadlock."
                          "It is recommended to use `mutex-lock` and `mutex-unlock` together"
                          "in a wrapper that guarantees unlocking, for example:"
                          ))
              (code '((define m (mutex-create))
                      (define with-mutex-do
                        (lambda (mutex expr)
                          (progn
                            (mutex-lock mutex)
                            (eval expr)
                            (mutex-unlock mutex))))
                      (with-mutex-do m '(+ 1 2))
                      ))
              end)))

(define entry-mutex-unlock
  (ref-entry "mutex-unlock"
             (list
              (para (list "`mutex-unlock` releases a mutex."
                          "The form of a `mutex-unlock` expression is `(mutex-unlock mutex)`."
                          "Must be called by the same process that holds the lock."
                          "Returns `t` on success and unblocks the next waiting process if any."
                          "Returns an error if the mutex is not currently locked."
                          ))
              (code '((define m (mutex-create))
                      (mutex-lock m)
                      (mutex-unlock m)
                      ))
              end)))

(define chapter-mutex
  (section 2 "Mutual Exclusion"
           (list entry-mutex-create
                 entry-mutex-lock
                 entry-mutex-unlock
                 )))

(define manual
  (list
   (section 1 "LispBM Mutex Extensions Reference Manual"
            (list
             (para (list "The mutex extensions provide mutual exclusion primitives for"
                         "synchronising concurrent LispBM processes."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             chapter-mutex
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "mutexref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Mutex extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
