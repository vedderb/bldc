; Test set extensions edge cases
; set-insert should take exactly 2 arguments: a list (set) and a value to insert
; set-union should take exactly 2 arguments: both must be lists (sets)

; set-insert with valid arguments (baseline behavior)
(define insert_valid_test1 (eq (set-insert '() 1) '(1)))
(define insert_valid_test2 (eq (set-insert '(1 2) 3) '(1 2 3)))
(define insert_valid_test3 (eq (set-insert '(1 2 3) 2) '(1 2 3))) ; no duplicates
(define insert_valid_test4 (eq (set-insert '(a b) 'c) '(a b c)))
(define insert_valid_test5 (eq (set-insert nil 42) '(42)))

; set-insert with wrong number of arguments
(define insert_args_test1 (eq (trap (set-insert)) '(exit-error type_error)))
(define insert_args_test2 (eq (trap (set-insert '(1 2))) '(exit-error type_error)))
(define insert_args_test3 (eq (trap (set-insert '(1) 2 3)) '(exit-error type_error)))
(define insert_args_test4 (eq (trap (set-insert '(1) 2 3 4 5)) '(exit-error type_error)))

; set-insert with non-list first argument
(define insert_type_test1 (eq (trap (set-insert 42 1)) '(exit-error type_error)))
(define insert_type_test2 (eq (trap (set-insert "string" 1)) '(exit-error type_error)))
(define insert_type_test3 (eq (trap (set-insert 'symbol 1)) '(exit-error type_error)))
(define insert_type_test4 (eq (trap (set-insert [1 2 3] 4)) '(exit-error type_error)))
(define insert_type_test5 (eq (trap (set-insert t 1)) '(exit-error type_error)))

; set-insert with any second argument type should work (sets can contain anything)
(define insert_val_test1 (list? (set-insert '() "string")))
(define insert_val_test2 (list? (set-insert '() 'symbol)))
(define insert_val_test3 (list? (set-insert '() nil)))
(define insert_val_test4 (list? (set-insert '() t)))
(define insert_val_test5 (list? (set-insert '() [1 2 3])))
(define insert_val_test6 (list? (set-insert '() '(nested list))))

; set-union with valid arguments (baseline behavior)
(define union_valid_test1 (eq (set-union '() '()) '()))
(define union_valid_test2 (eq (set-union '(1) '(2)) '(2 1)))
(define union_valid_test3 (eq (set-union '(1 2) '(2 3)) '(2 3 1))) ; no duplicates
(define union_valid_test4 (eq (set-union nil nil) nil))
(define union_valid_test5 (eq (set-union '(a b) '(c d)) '(c d a b)))

; set-union with wrong number of arguments
(define union_args_test1 (eq (trap (set-union)) '(exit-error type_error)))
(define union_args_test2 (eq (trap (set-union '(1 2))) '(exit-error type_error)))
(define union_args_test3 (eq (trap (set-union '(1) '(2) '(3))) '(exit-error type_error)))
(define union_args_test4 (eq (trap (set-union '(1) '(2) '(3) '(4) '(5))) '(exit-error type_error)))

; set-union with non-list first argument
(define union_type1_test1 (eq (trap (set-union 42 '(1 2))) '(exit-error type_error)))
(define union_type1_test2 (eq (trap (set-union "string" '(1 2))) '(exit-error type_error)))
(define union_type1_test3 (eq (trap (set-union 'symbol '(1 2))) '(exit-error type_error)))
(define union_type1_test4 (eq (trap (set-union [1 2] '(3 4))) '(exit-error type_error)))
(define union_type1_test5 (eq (trap (set-union t '(1 2))) '(exit-error type_error)))

; set-union with non-list second argument
(define union_type2_test1 (eq (trap (set-union '(1 2) 42)) '(exit-error type_error)))
(define union_type2_test2 (eq (trap (set-union '(1 2) "string")) '(exit-error type_error)))
(define union_type2_test3 (eq (trap (set-union '(1 2) 'symbol)) '(exit-error type_error)))
(define union_type2_test4 (eq (trap (set-union '(1 2) [3 4])) '(exit-error type_error)))
(define union_type2_test5 (eq (trap (set-union '(1 2) t)) '(exit-error type_error)))

; set-union with both arguments non-lists
(define union_both_test1 (eq (trap (set-union 42 "string")) '(exit-error type_error)))
(define union_both_test2 (eq (trap (set-union 'sym1 'sym2)) '(exit-error type_error)))
(define union_both_test3 (eq (trap (set-union [1] [2])) '(exit-error type_error)))

; Test return types and basic properties
(define return_test1 (list? (set-insert '(1 2) 3)))
(define return_test2 (list? (set-union '(1) '(2))))
(define return_test3 (eq (length (set-insert '(1 2 3) 2)) 3)) ; no duplicate added
(define return_test4 (eq (length (set-insert '(1 2 3) 4)) 4)) ; new element added

; Edge case: inserting complex values
(define complex_test1 (list? (set-insert '() '(nested list))))
(define complex_test2 (list? (set-insert '("a") "b")))
(define complex_test3 (list? (set-union '((a 1)) '((b 2)))))

(if (and insert_valid_test1 insert_valid_test2 insert_valid_test3 insert_valid_test4 insert_valid_test5
         insert_args_test1 insert_args_test2 insert_args_test3 insert_args_test4
         insert_type_test1 insert_type_test2 insert_type_test3 insert_type_test4 insert_type_test5
         insert_val_test1 insert_val_test2 insert_val_test3 insert_val_test4 insert_val_test5 insert_val_test6
         union_valid_test1 union_valid_test2 union_valid_test3 union_valid_test4 union_valid_test5
         union_args_test1 union_args_test2 union_args_test3 union_args_test4
         union_type1_test1 union_type1_test2 union_type1_test3 union_type1_test4 union_type1_test5
         union_type2_test1 union_type2_test2 union_type2_test3 union_type2_test4 union_type2_test5
         union_both_test1 union_both_test2 union_both_test3
         return_test1 return_test2 return_test3 return_test4
         complex_test1 complex_test2 complex_test3)
    (print "SUCCESS")
    (print "FAILURE"))