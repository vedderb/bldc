(define a 12)
(defun b () 18)

(define res 
    (let ((a 9))
        (+ (b) (let ((b 6))
            (progn
                (setvar 'b 7)
                (setvar 'a (+ b 2))
                a))
        a)
))

; Expected:
; res = 36
; a   = 12
; (b) = 18

(check (and (= res 36)
            (= a 12)
            (= (b) 18)))
