
; Create interconnected cycles - more complex graph structure
(define node1 (list 1 nil))
(define node2 (list 2 nil))
(define node3 (list 3 nil))
(define node4 (list 4 nil))
(define node5 (list 5 nil))

; Create two interconnected cycles:
; Cycle A: node1 -> node2 -> node3 -> node1
; Cycle B: node3 -> node4 -> node5 -> node3
; node3 is shared between both cycles

(setcar (cdr node1) node2)
(setcar (cdr node2) node3)
(setcar (cdr node3) node1)  ; Complete first cycle

(define node3b (list 3 nil))
(setcar (cdr node3b) node4)
(setcar (cdr node4) node5)
(setcar (cdr node5) node3b) ; Complete second cycle

(define graph (list node1 node3b))

(defun get-val (node) (car node))
(defun get-next (node) (car (cdr node)))

(defun main () {
       (if (and ; Verify first cycle through node1
                (= (get-val node1) 1)
                (= (get-val (get-next node1)) 2)
                (= (get-val (get-next (get-next node1))) 3)
                (= (get-val (get-next (get-next (get-next node1)))) 1)
                ; Verify second cycle through node3b
                (= (get-val node3b) 3)
                (= (get-val (get-next node3b)) 4)
                (= (get-val (get-next (get-next node3b))) 5)
                (= (get-val (get-next (get-next (get-next node3b)))) 3))
           (print "SUCCESS")
           (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
