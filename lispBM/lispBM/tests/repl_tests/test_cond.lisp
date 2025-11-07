; Test cond form functionality and edge cases

; Basic cond functionality
(define basic_test1 (eq (cond ((< 5 10) 'first-branch)
                              ((> 5 10) 'second-branch)
                              (t 'default-branch))
                        'first-branch))

(define basic_test2 (eq (cond ((> 5 10) 'first-branch) 
                              ((< 5 0) 'second-branch)
                              ((= 5 5) 'third-branch)
                              (t 'default-branch))
                        'third-branch))

(define basic_test3 (eq (cond ((> 5 10) 'first-branch)
                              ((< 5 0) 'second-branch)
                              (t 'default-branch))
                        'default-branch))

; Multiple conditions with numbers
(define num_test1 (eq (cond ((= 0 0) 'zero)
                            ((= 1 1) 'one) 
                            (t 'other))
                      'zero))

(define num_test2 (eq (cond ((= 0 1) 'not-zero)
                            ((= 1 1) 'one)
                            (t 'other))
                      'one))

; Test with different data types in conditions
(define type_test1 (eq (cond ((eq nil nil) 'nil-match)
                             ((eq t t) 'true-match)
                             (t 'no-match))
                       'nil-match))

(define type_test2 (eq (cond ((eq 'symbol 'different) 'no-match)
                             ((eq 'symbol 'symbol) 'symbol-match)
                             (t 'default))
                       'symbol-match))

; Test with list conditions
(define list_test1 (eq (cond ((eq '(1 2) '(1 2)) 'list-match)
                             (t 'no-match))
                       'list-match))

(define list_test2 (eq (cond ((eq '() nil) 'empty-list)
                             (t 'not-empty))
                       'empty-list))

; Test when no conditions match (should return nil)
(define nil_test1 (eq (cond ((= 1 2) 'impossible)
                            ((= 3 4) 'also-impossible)
                            ((= 5 6) 'still-impossible))
                      nil))

; Test nested cond expressions
(define nested_test1 (eq (cond ((= 1 1) (cond ((= 2 2) 'nested-match)
                                             (t 'inner-default)))
                              (t 'outer-default))
                         'nested-match))

; Test with complex expressions in conditions
(define complex_test1 (eq (cond ((and (= 1 1) (= 2 2)) 'and-match)
                                (t 'no-match))
                          'and-match))

(define complex_test2 (eq (cond ((or (= 1 2) (= 2 2)) 'or-match)
                                (t 'no-match))
                          'or-match))

; Test with function calls in conditions
(define func_test1 (eq (cond ((list? '(1 2 3)) 'is-list)
                             (t 'not-list))
                       'is-list))

(define func_test2 (eq (cond ((number? 42) 'is-number)
                             (t 'not-number))
                       'is-number))

; Test with arithmetic in conditions  
(define arith_test1 (eq (cond ((> (+ 2 3) 4) 'sum-greater)
                              (t 'sum-not-greater))
                        'sum-greater))

; Test single condition
(define single_test1 (eq (cond ((= 1 1) 'single-match))
                         'single-match))

(define single_test2 (eq (cond ((= 1 2) 'no-match))
                         nil))

; Test with variable references
(define test_var 42)
(define var_test1 (eq (cond ((= test_var 42) 'var-match)
                            (t 'no-match))
                      'var-match))

; Test condition evaluation order
(define order_counter 0)
(define order_test1 
  (progn
    (setq order_counter 0)
    (eq (cond ((progn (setq order_counter (+ order_counter 1)) (= 1 2)) 'first)
              ((progn (setq order_counter (+ order_counter 10)) (= 2 2)) 'second)
              ((progn (setq order_counter (+ order_counter 100)) t) 'third))
        'second)))

(define order_test2 (= order_counter 11)) ; Should be 1 + 10, third condition not evaluated

; Test with boolean expressions
(define bool_test1 (eq (cond (t 'true-literal)
                             (nil 'nil-literal))
                       'true-literal))

(define bool_test2 (eq (cond (nil 'nil-literal)
                             (t 'true-literal))
                       'true-literal))

; Test edge cases - cond with various edge conditions
(define edge_test1 (eq (trap (cond)) '(exit-ok nil)))
(define edge_test2 (eq (trap (cond ())) '(exit-ok nil)))
(define edge_test3 (eq (trap (cond 't)) '(exit-ok t)))

; Test with non-boolean conditions that evaluate to truthy/falsy
(define truthy_test1 (eq (cond (1 'number-truthy)
                               (t 'default))
                         'number-truthy))

(define truthy_test2 (eq (cond ('symbol 'symbol-truthy)
                               (t 'default))
                         'symbol-truthy))

(define truthy_test3 (eq (cond ('(1 2 3) 'list-truthy)
                               (t 'default))
                         'list-truthy))

; Test falsy values
(define falsy_test1 (eq (cond (nil 'nil-falsy)
                              (t 'default))
                        'default))


(define select-branch
    (lambda (i)
      (cond ((= i 0) 0)
            ((= i 1) 1)
            ((= i 2) 2)
            ((= i 3) 3)
            ((= i 4) 4)
            ((= i 5) 5)
            ((= i 6) 6)
            ((= i 7) 7)
            ((= i 8) 8)
            ((= i 9) 9)
            ((= i 10) 10)
            ((= i 11) 11)
            ((= i 12) 12)
            ((= i 13) 13)
            ((= i 14) 14)
            ((= i 15) 15)
            ((= i 16) 16)
            ((= i 17) 17)
            ((= i 18) 18)
            ((= i 19) 19)
            ((= i 20) 20)
            ((= i 21) 21)
            ((= i 22) 22)
            ((= i 23) 23)
            ((= i 24) 24)
            ((= i 25) 25)
            ((= i 26) 26)
            ((= i 27) 27)
            ((= i 28) 28)
            ((= i 29) 29)
            ((= i 30) 30)
            ((= i 31) 31)
            ((= i 32) 32)
            ((= i 33) 33)
            ((= i 34) 34)
            ((= i 35) 35)
            ((= i 36) 36)
            ((= i 37) 37)
            ((= i 38) 38)
            ((= i 39) 39)
            ((= i 40) 40)
            ((= i 41) 41)
            ((= i 42) 42)
            ((= i 43) 43)
            ((= i 44) 44)
            ((= i 45) 45)
            ((= i 46) 46)
            ((= i 47) 47)
            ((= i 48) 48)
            ((= i 49) 49))))

(define lots_of_branches_test1 {
  (var ok t)
  (looprange i 0 50
        (setq ok (and (= (select-branch i) i))))
  ok
  })

; Test with 37 composite conditions - each condition uses complex expressions
(define composite-conditions-func
  (lambda (i)
    (cond 
      ((and (= i 0) (>= 0 0) (= (mod 0 2) 0)) 'case-0)
      ((and (= i 1) (or (= 1 1) (= 1 2))) 'case-1) 
      ((and (= i 2) (< 2 5) (> 2 1)) 'case-2)
      ((and (= i 3) (= (+ 1 2) 3) (not (= 3 4))) 'case-3)
      ((and (= i 4) (>= 4 4) (<= 4 10)) 'case-4)
      ((and (= i 5) (= (* 5 2) 10) (> 5 0)) 'case-5)
      ((and (= i 6) (= (// 6 2) 3) (= (mod 6 3) 0)) 'case-6)
      ((and (= i 7) (or (= 7 7) (= 7 8)) (< 7 10)) 'case-7)
      ((and (= i 8) (= (- 8 0) 8) (not (= 8 9))) 'case-8)
      ((and (= i 9) (>= 9 5) (= (mod 9 3) 0)) 'case-9)
      ((and (= i 10) (= (+ 5 5) 10) (< 10 20)) 'case-10)
      ((and (= i 11) (or (= 11 11) (= 11 12)) (> 11 10)) 'case-11)
      ((and (= i 12) (= (// 12 3) 4) (>= 12 10)) 'case-12)
      ((and (= i 13) (= (* 13 1) 13) (<= 13 15)) 'case-13)
      ((and (= i 14) (= (- 14 4) 10) (not (= 14 15))) 'case-14)
      ((and (= i 15) (= (mod 15 5) 0) (> 15 10)) 'case-15)
      ((and (= i 16) (= (// 16 2) 8) (< 16 20)) 'case-16)
      ((and (= i 17) (or (= 17 17) (= 17 18)) (>= 17 15)) 'case-17)
      ((and (= i 18) (= (+ 9 9) 18) (= (mod 18 2) 0)) 'case-18)
      ((and (= i 19) (= (* 19 1) 19) (<= 19 25)) 'case-19)
      ((and (= i 20) (= (// 20 4) 5) (not (= 20 21))) 'case-20)
      ((and (= i 21) (= (mod 21 7) 0) (> 21 20)) 'case-21)
      ((and (= i 22) (= (- 22 2) 20) (< 22 25)) 'case-22)
      ((and (= i 23) (or (= 23 23) (= 23 24)) (>= 23 20)) 'case-23)
      ((and (= i 24) (= (+ 12 12) 24) (= (mod 24 3) 0)) 'case-24)
      ((and (= i 25) (= (* 25 1) 25) (<= 25 30)) 'case-25)
      ((and (= i 26) (= (// 26 2) 13) (not (= 26 27))) 'case-26)
      ((and (= i 27) (= (mod 27 9) 0) (> 27 25)) 'case-27)
      ((and (= i 28) (= (- 28 8) 20) (< 28 30)) 'case-28)
      ((and (= i 29) (or (= 29 29) (= 29 30)) (>= 29 25)) 'case-29)
      ((and (= i 30) (= (+ 15 15) 30) (= (mod 30 5) 0)) 'case-30)
      ((and (= i 31) (= (* 31 1) 31) (<= 31 35)) 'case-31)
      ((and (= i 32) (= (// 32 4) 8) (not (= 32 33))) 'case-32)
      ((and (= i 33) (= (mod 33 11) 0) (> 33 30)) 'case-33)
      ((and (= i 34) (= (- 34 4) 30) (< 34 40)) 'case-34)
      ((and (= i 35) (or (= 35 35) (= 35 36)) (>= 35 30)) 'case-35)
      ((and (= i 36) (= (+ 18 18) 36) (= (mod 36 6) 0)) 'case-36)
      (t 'no-match))))

; Test that verifies each composite case can be reached (0-36 = 37 cases)
(define composite_conditions_test {
  (var ok t)
  (looprange i 0 37
    (setq ok (and ok (eq (composite-conditions-func i) (ix '(case-0 case-1 case-2 case-3 case-4 case-5 case-6 case-7 case-8 case-9 case-10 case-11 case-12 case-13 case-14 case-15 case-16 case-17 case-18 case-19 case-20 case-21 case-22 case-23 case-24 case-25 case-26 case-27 case-28 case-29 case-30 case-31 case-32 case-33 case-34 case-35 case-36) i)))))
  ok
  })

; Test with 13 branches where each branch executes a {} block of operations
(define block-operations-test
  (lambda (n)
    (cond 
      ((= n 0) {
        (var result 0)
        (setq result (+ result 10))
        (setq result (* result 2))
        result
      })
      ((= n 1) {
        (var x 5)
        (var y 3)
        (+ (* x y) 1)
      })
      ((= n 2) {
        (var my-list '(1 2 3))
        (var sum 0)
        (setq sum (+ sum (car my-list)))
        (setq my-list (cdr my-list))
        (setq sum (+ sum (car my-list)))
        sum
      })
      ((= n 3) {
        (var counter 0)
        (setq counter (+ counter 1))
        (setq counter (+ counter 2))
        (setq counter (+ counter 3))
        counter
      })
      ((= n 4) {
        (var a 4)
        (var b 2)
        (var quotient (// a b))
        (var remainder (mod a b))
        (+ quotient remainder)
      })
      ((= n 5) {
        (var str "test")
        (var num 42)
        (var bool t)
        (if bool num 0)
      })
      ((= n 6) {
        (var nested-list '((1 2) (3 4)))
        (var first-pair (car nested-list))
        (var second-element (car (cdr first-pair)))
        second-element
      })
      ((= n 7) {
        (var operations '())
        (setq operations (cons 1 operations))
        (setq operations (cons 2 operations))
        (setq operations (cons 3 operations))
        (length operations)
      })
      ((= n 8) {
        (var base 2)
        (var exponent 3)
        (var result 1)
        (setq result (* result base))
        (setq result (* result base))
        (setq result (* result base))
        result
      })
      ((= n 9) {
        (var data '(a b c d))
        (var filtered '())
        (setq filtered (cons (car data) filtered))
        (setq data (cdr (cdr data)))
        (setq filtered (cons (car data) filtered))
        (length filtered)
      })
      ((= n 10) {
        (var matrix '((1 2) (3 4)))
        (var row1 (car matrix))
        (var row2 (car (cdr matrix)))
        (var sum (+ (car row1) (car row2)))
        sum
      })
      ((= n 11) {
        (var flags '(t nil t nil))
        (var true-count 0)
        (if (car flags) (setq true-count (+ true-count 1)))
        (setq flags (cdr (cdr flags)))
        (if (car flags) (setq true-count (+ true-count 1)))
        true-count
      })
      ((= n 12) {
        (var numbers '(10 20 30))
        (var product 1)
        (setq product (* product (car numbers)))
        (setq numbers (cdr numbers))
        (setq product (* product (car numbers)))
        (// product 10)
      })
      (t 'no-match))))

; Test that verifies each block operation case works correctly
(define block_operations_test {
  (var all-correct t)
  (setq all-correct (and all-correct (= (block-operations-test 0) 20)))   ; (0 + 10) * 2 = 20
  (setq all-correct (and all-correct (= (block-operations-test 1) 16)))   ; 5 * 3 + 1 = 16
  (setq all-correct (and all-correct (= (block-operations-test 2) 3)))    ; 1 + 2 = 3
  (setq all-correct (and all-correct (= (block-operations-test 3) 6)))    ; 1 + 2 + 3 = 6
  (setq all-correct (and all-correct (= (block-operations-test 4) 2)))    ; 4//2 + 4%2 = 2 + 0 = 2
  (setq all-correct (and all-correct (= (block-operations-test 5) 42)))   ; if t then 42 else 0 = 42
  (setq all-correct (and all-correct (= (block-operations-test 6) 2)))    ; second element of (1 2) = 2
  (setq all-correct (and all-correct (= (block-operations-test 7) 3)))    ; length of (3 2 1) = 3
  (setq all-correct (and all-correct (= (block-operations-test 8) 8)))    ; 2^3 = 8
  (setq all-correct (and all-correct (= (block-operations-test 9) 2)))    ; length of (c a) = 2
  (setq all-correct (and all-correct (= (block-operations-test 10) 4)))   ; 1 + 3 = 4
  (setq all-correct (and all-correct (= (block-operations-test 11) 2)))   ; count of t flags = 2
  (setq all-correct (and all-correct (= (block-operations-test 12) 20)))  ; (10 * 20) / 10 = 20
  all-correct
  })


; Comprehensive success check
(if (and basic_test1 basic_test2 basic_test3
         num_test1 num_test2
         type_test1 type_test2
         list_test1 list_test2
         nil_test1
         nested_test1
         complex_test1 complex_test2
         func_test1 func_test2
         arith_test1
         single_test1 single_test2
         var_test1
         order_test1 order_test2
         bool_test1 bool_test2
         edge_test1 edge_test2 edge_test3
         truthy_test1 truthy_test2 truthy_test3
         falsy_test1
         lots_of_branches_test1
         composite_conditions_test
         block_operations_test)
    (print "SUCCESS")
    (print "FAILURE"))
