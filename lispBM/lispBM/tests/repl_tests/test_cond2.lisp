; Test nested cond expressions with 3 branches per level

; Test nested cond expressions with 3 branches per level
(define nested-cond-test
  (lambda (level1 level2)
    (cond 
      ((= level1 0) 
       (cond 
         ((= level2 0) 'branch-0-0)
         ((= level2 1) 'branch-0-1) 
         ((= level2 2) 'branch-0-2)))
      ((= level1 1)
       (cond
         ((= level2 0) 'branch-1-0)
         ((= level2 1) 'branch-1-1)
         ((= level2 2) 'branch-1-2)))
      ((= level1 2)
       (cond
         ((= level2 0) 'branch-2-0)
         ((= level2 1) 'branch-2-1)
         ((= level2 2) 'branch-2-2))))))

; Test triple-nested cond expressions (3 levels deep)
(define triple-nested-cond-test
  (lambda (l1 l2 l3)
    (cond
      ((= l1 0)
       (cond
         ((= l2 0)
          (cond
            ((= l3 0) 'deep-0-0-0)
            ((= l3 1) 'deep-0-0-1)
            ((= l3 2) 'deep-0-0-2)))
         ((= l2 1)
          (cond
            ((= l3 0) 'deep-0-1-0)
            ((= l3 1) 'deep-0-1-1)
            ((= l3 2) 'deep-0-1-2)))
         ((= l2 2)
          (cond
            ((= l3 0) 'deep-0-2-0)
            ((= l3 1) 'deep-0-2-1)
            ((= l3 2) 'deep-0-2-2)))))
      ((= l1 1)
       (cond
         ((= l2 0)
          (cond
            ((= l3 0) 'deep-1-0-0)
            ((= l3 1) 'deep-1-0-1)
            ((= l3 2) 'deep-1-0-2)))
         ((= l2 1)
          (cond
            ((= l3 0) 'deep-1-1-0)
            ((= l3 1) 'deep-1-1-1)
            ((= l3 2) 'deep-1-1-2)))
         ((= l2 2)
          (cond
            ((= l3 0) 'deep-1-2-0)
            ((= l3 1) 'deep-1-2-1)
            ((= l3 2) 'deep-1-2-2)))))
      ((= l1 2)
       (cond
         ((= l2 0)
          (cond
            ((= l3 0) 'deep-2-0-0)
            ((= l3 1) 'deep-2-0-1)
            ((= l3 2) 'deep-2-0-2)))
         ((= l2 1)
          (cond
            ((= l3 0) 'deep-2-1-0)
            ((= l3 1) 'deep-2-1-1)
            ((= l3 2) 'deep-2-1-2)))
         ((= l2 2)
          (cond
            ((= l3 0) 'deep-2-2-0)
            ((= l3 1) 'deep-2-2-1)
            ((= l3 2) 'deep-2-2-2))))))))

; Test nested cond with different branch types (operations, lists, arithmetic)
(define mixed-nested-cond-test
  (lambda (outer inner)
    (cond
      ((= outer 0)
       (cond
         ((= inner 0) (+ 10 20))       ; Arithmetic in branch
         ((= inner 1) '(a b c))        ; List in branch  
         ((= inner 2) (* inner 5))))   ; Variable arithmetic
      ((= outer 1)
       (cond
         ((= inner 0) { (var x 42) (+ x 8) })  ; Block operations
         ((= inner 1) (length '(1 2 3 4)))     ; Function call
         ((= inner 2) (car '(first second)))))  ; List operation
      ((= outer 2)
       (cond
         ((= inner 0) (if (> 5 3) 'true-case 'false-case))  ; Nested conditional
         ((= inner 1) (and t (or nil t)))                    ; Boolean logic
         ((= inner 2) (mod (+ inner 10) 3)))))))             ; Complex arithmetic

; Test that verifies all nested cond combinations work
(define nested_cond_tests {
  (var all-passed t)
  
  ; Test 2-level nesting (3x3 = 9 combinations)
  (setq all-passed (and all-passed (eq (nested-cond-test 0 0) 'branch-0-0)))
  (setq all-passed (and all-passed (eq (nested-cond-test 0 1) 'branch-0-1)))
  (setq all-passed (and all-passed (eq (nested-cond-test 0 2) 'branch-0-2)))
  (setq all-passed (and all-passed (eq (nested-cond-test 1 0) 'branch-1-0)))
  (setq all-passed (and all-passed (eq (nested-cond-test 1 1) 'branch-1-1)))
  (setq all-passed (and all-passed (eq (nested-cond-test 1 2) 'branch-1-2)))
  (setq all-passed (and all-passed (eq (nested-cond-test 2 0) 'branch-2-0)))
  (setq all-passed (and all-passed (eq (nested-cond-test 2 1) 'branch-2-1)))
  (setq all-passed (and all-passed (eq (nested-cond-test 2 2) 'branch-2-2)))
  
  ; Test 3-level nesting (3x3x3 = 27 combinations - testing key ones)
  (setq all-passed (and all-passed (eq (triple-nested-cond-test 0 0 0) 'deep-0-0-0)))
  (setq all-passed (and all-passed (eq (triple-nested-cond-test 0 1 2) 'deep-0-1-2)))
  (setq all-passed (and all-passed (eq (triple-nested-cond-test 1 0 1) 'deep-1-0-1)))
  (setq all-passed (and all-passed (eq (triple-nested-cond-test 1 2 0) 'deep-1-2-0)))
  (setq all-passed (and all-passed (eq (triple-nested-cond-test 2 1 1) 'deep-2-1-1)))
  (setq all-passed (and all-passed (eq (triple-nested-cond-test 2 2 2) 'deep-2-2-2)))
  
  ; Test mixed nested operations
  (setq all-passed (and all-passed (= (mixed-nested-cond-test 0 0) 30)))        ; 10 + 20 = 30
  (setq all-passed (and all-passed (eq (mixed-nested-cond-test 0 1) '(a b c))))  ; List result
  (setq all-passed (and all-passed (= (mixed-nested-cond-test 0 2) 10)))        ; 2 * 5 = 10
  (setq all-passed (and all-passed (= (mixed-nested-cond-test 1 0) 50)))        ; 42 + 8 = 50
  (setq all-passed (and all-passed (= (mixed-nested-cond-test 1 1) 4)))         ; length of (1 2 3 4) = 4
  (setq all-passed (and all-passed (eq (mixed-nested-cond-test 1 2) 'first)))   ; car of (first second) = first
  (setq all-passed (and all-passed (eq (mixed-nested-cond-test 2 0) 'true-case))) ; if 5>3 = true-case
  (setq all-passed (and all-passed (eq (mixed-nested-cond-test 2 1) t)))        ; and t (or nil t) = t
  (setq all-passed (and all-passed (= (mixed-nested-cond-test 2 2) 0)))         ; (2+10) mod 3 = 0
  
  all-passed
  })

; Success check
(if nested_cond_tests
    (print "SUCCESS")
    (print "FAILURE"))