
; Create a shared sublist
(define shared (list 'shared 'data))

; Create two circular lists that both reference the shared sublist
(define cycle1 (list 'a shared 'c))
(define cycle2 (list 1 shared 3 4))

; Make them circular
(setcdr (cdr (cdr cycle1)) cycle1)           ; cycle1: a->shared->c->a
(setcdr (cdr (cdr (cdr cycle2))) cycle2)     ; cycle2: 1->shared->3->4->1

; Create a structure that references both cycles (more sharing)
(define combined (list cycle1 cycle2 cycle1 cycle2))

(defun main () {
       (if (and ; Verify cycle1 structure and circularity by checking indices
                (eq (ix cycle1 0) 'a)
                (eq (ix (ix cycle1 1) 0) 'shared)
                (eq (ix cycle1 2) 'c)
                (eq (ix cycle1 3) 'a)
                (eq (ix cycle1 4) (ix cycle1 1))
                ; Verify cycle2 structure and circularity
                (= (ix cycle2 0) 1)
                (eq (ix (ix cycle2 1) 0) 'shared)
                (= (ix cycle2 2) 3)
                (= (ix cycle2 3) 4)
                (= (ix cycle2 4) 1)
                (eq (ix cycle2 5) (ix cycle2 1))
                ; Verify shared sublist is the same object in both cycles
                (eq (ix cycle1 1) (ix cycle2 1))
                                        ; Verify combined list structure (check it has the right elements at indices)
                (eq (ix (ix combined 0) 0) 'a)
                (= (ix (ix combined 1) 0) 1))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
