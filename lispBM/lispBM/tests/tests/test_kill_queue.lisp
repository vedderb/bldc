; Checks that kill doesn't also kill threads in the running queue.
; This would be caused by a bug which has now been fixed.

; This will always be in the running queue when the main context is executing.
(def no-sleep-ctx (spawn "no-sleep" 50 (fn () (loopwhile t nil))))
; This will always be in the blocked queue.
(def always-sleep-ctx (spawn "always-sleep" 50 (fn () (sleep 100))))

(kill always-sleep-ctx nil)

; Check that no-sleep hasn't been killed. `send` will return `nil` if it's dead.
(check (send no-sleep-ctx nil))
