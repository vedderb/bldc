
; Create multiple independent cycles
(define cycle1 (list 'a 'b 'c))
(define cycle2 (list 1 2 3 4))
(define cycle3 (list 'x 'y))

; Make each one circular
(setcdr (cdr (cdr cycle1)) cycle1)           ; cycle1: a->b->c->a
(setcdr (cdr (cdr (cdr cycle2))) cycle2)     ; cycle2: 1->2->3->4->1
(setcdr (cdr cycle3) cycle3)                 ; cycle3: x->y->x

; Create a structure that references all three cycles
(define multi (list cycle1 cycle2 cycle3))

(defun main () {
       (if (and ; Verify cycle1
                (eq (ix cycle1 0) 'a)
                (eq (ix cycle1 1) 'b)
                (eq (ix cycle1 2) 'c)
                (eq (ix cycle1 3) 'a)
                ; Verify cycle2
                (eq (ix cycle2 0) 1)
                (eq (ix cycle2 1) 2)
                (eq (ix cycle2 2) 3)
                (eq (ix cycle2 3) 4)
                (eq (ix cycle2 4) 1)
                ; Verify cycle3
                (eq (ix cycle3 0) 'x)
                (eq (ix cycle3 1) 'y)
                (eq (ix cycle3 2) 'x)
                ; Verify multi references them
                (eq (ix (ix multi 0) 0) 'a)
                (eq (ix (ix multi 1) 0) 1)
                (eq (ix (ix multi 2) 0) 'x))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
