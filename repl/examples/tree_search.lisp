;; Tree search adapted from lisp in small pieces
(defun make-node (value left right)
  (list value left right))

(define tree
  (make-node 5
            (make-node 3
                      (make-node 1 nil nil)
                      (make-node 4 nil nil))
            (make-node 8
                      (make-node 7 nil nil)
                      (make-node 9 nil nil))))

(defun left-tree (x)
  (car (cdr x)))
(defun right-tree (x)
  (car (cdr (cdr x))))

(defun search (tree n)
  (if (eq tree NIL) nil
    (if (= (car tree) n)
        't
      (or (search (left-tree tree) n)
          (search (right-tree tree) n)))))

(defun search-ret (tree n)
  (if (eq tree NIL) nil
    (if (= (car tree) n)
        (pop-ret 't) ;; saves a lot of oring on the way up from discovery.
      (or (search (left-tree tree) n)
          (search (right-tree tree) n)))))


(defun search-efficient (tree n)
  (push-ret (search-ret tree n)))

