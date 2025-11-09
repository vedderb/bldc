
; Create shared sublists
(define shared1 (list 'x 'y))
(define shared2 (list 10 20 30))

; Create circular structures with sharing
(define cycle1 (list shared1 shared2 'mid))
(define cycle2 (list 'start shared1 shared2))

; Make them circular
(setcdr (cdr (cdr cycle1)) cycle1)           ; cycle1: shared1->shared2->mid->shared1
(setcdr (cdr (cdr cycle2)) cycle2)           ; cycle2: start->shared1->shared2->start

; Create a third cycle that shares with the others
(define cycle3 (list shared1 'between shared2))
(setcdr (cdr (cdr cycle3)) cycle3)           ; cycle3: shared1->between->shared2->shared1

(define all-cycles (list cycle1 cycle2 cycle3))

(defun main () {
       (if (and ; Verify shared1 is the same across all cycles
                (eq (ix cycle1 0) shared1)
                (eq (ix cycle2 1) shared1)
                (eq (ix cycle3 0) shared1)
                (eq (ix cycle1 0) (ix cycle2 1))
                (eq (ix cycle2 1) (ix cycle3 0))
                ; Verify shared2 is the same across all cycles
                (eq (ix cycle1 1) shared2)
                (eq (ix cycle2 2) shared2)
                (eq (ix cycle3 2) shared2)
                (eq (ix cycle1 1) (ix cycle2 2))
                ; Verify circularity of cycle1
                (eq (ix cycle1 0) shared1)
                (eq (ix cycle1 3) shared1)
                ; Verify circularity of cycle2
                (eq (ix cycle2 0) 'start)
                (eq (ix cycle2 3) 'start)
                ; Verify circularity of cycle3
                (eq (ix cycle3 1) 'between)
                (eq (ix cycle3 4) 'between))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
