; Test type checking and conversion operations with incorrect types and argument counts

; type-of edge cases
(define typeof_test1 (eq (trap (type-of)) '(exit-error eval_error)))
(define typeof_test2 (eq (type-of 42) 'type-i))
(define typeof_test3 (eq (type-of "string") 'type-array))
(define typeof_test4 (eq (type-of 'symbol) 'type-symbol))
(define typeof_test5 (eq (type-of '(1 2)) 'type-list))
(define typeof_test6 (eq (type-of nil) 'type-symbol))
(define typeof_test7 (eq (type-of t) 'type-symbol))
(define typeof_test8 (eq (trap (type-of 1 2)) '(exit-error eval_error)))

; is-list edge cases
(define is_list_test1 (eq (trap (list?)) '(exit-error type_error)))
(define is_list_test2 (eq (list? '(1 2 3)) t))
(define is_list_test3 (eq (list? nil) t))
(define is_list_test4 (eq (list? 42) nil))
(define is_list_test5 (eq (list? "string") nil))
(define is_list_test6 (eq (list? 'symbol) nil))
(define is_list_test7 (eq (list? t) nil))
(define is_list_test8 (eq (list? [1 2 3]) nil))
(define is_list_test9 (eq (trap (list? '(1) 2)) '(exit-error type_error)))

; is-number edge cases
(define is_number_test1 (eq (trap (number?)) '(exit-error type_error)))
(define is_number_test2 (eq (number? 42) t))
(define is_number_test3 (eq (number? 3.14) t))
(define is_number_test4 (eq (number? "string") nil))
(define is_number_test5 (eq (number? 'symbol) nil))
(define is_number_test6 (eq (number? '(1 2)) nil))
(define is_number_test7 (eq (number? nil) nil))
(define is_number_test8 (eq (number? t) nil))
(define is_number_test9 (eq (number? [1 2]) nil))
(define is_number_test10 (eq (trap (number? 42 3.14)) '(exit-error type_error)))

; Type conversion edge cases - to-i
(define to_i_test1 (eq (trap (to-i)) '(exit-error eval_error)))
(define to_i_test2 (eq (to-i 3.14) 3))
(define to_i_test3 (eq (to-i 42) 42))
(define to_i_test4 (eq (to-i "string") 0))
(define to_i_test5 (eq (to-i 'symbol) 0))
(define to_i_test6 (eq (to-i '(1)) 0))
(define to_i_test7 (eq (to-i nil) 0))
(define to_i_test8 (eq (to-i t) 0))
(define to_i_test9 (eq (to-i [1]) 0))
(define to_i_test10 (eq (trap (to-i 42 3.14)) '(exit-error eval_error)))

; Type conversion edge cases - to-float
(define to_float_test1 (eq (trap (to-float)) '(exit-error eval_error)))
(define to_float_test2 (eq (to-float 42) 42.0))
(define to_float_test3 (eq (to-float 3.14) 3.14))
(define to_float_test4 (eq (to-float "string") 0.0f32)) 
(define to_float_test5 (eq (to-float 'symbol) 0.0f32)) 
(define to_float_test6 (eq (to-float '(1)) 0.0f32)) 
(define to_float_test7 (eq (to-float nil) 0.0f32)) 
(define to_float_test8 (eq (to-float t) 0.0f32)) 
(define to_float_test9 (eq (to-float [1]) 0.0f32)) 
(define to_float_test10 (eq (trap (to-float 42 3.14)) '(exit-error eval_error)))

; Symbol to string conversion edge cases
(define sym_to_str_test1 (eq (trap (sym2str)) '(exit-error eval_error)))
(define sym_to_str_test2 (eq (sym2str 'hello) "hello"))
(define sym_to_str_test3 (eq (trap (sym2str 42)) '(exit-error type_error)))
(define sym_to_str_test4 (eq (trap (sym2str "string")) '(exit-error type_error)))
(define sym_to_str_test5 (eq (trap (sym2str '(1))) '(exit-error type_error)))
(define sym_to_str_test6 (eq (trap (sym2str [1])) '(exit-error type_error)))
(define sym_to_str_test7 (eq (trap (sym2str 'hello 'world)) '(exit-error eval_error)))

; String to symbol conversion edge cases
(define str_to_sym_test1 (eq (trap (str2sym)) '(exit-error eval_error)))
(define str_to_sym_test2 (eq (str2sym "hello") 'hello))
(define str_to_sym_test3 (eq (trap (str2sym 42)) '(exit-error type_error)))
(define str_to_sym_test4 (eq (trap (str2sym 'symbol)) '(exit-error type_error)))
(define str_to_sym_test5 (eq (trap (str2sym '(1))) '(exit-error type_error)))
;; TODO: bit dangerous behaviour. look into a sollution.
;;(define str_to_sym_test6 (eq (trap (str2sym [1])) '(exit-error type_error)))
(define str_to_sym_test7 (eq (trap (str2sym "hello" "world")) '(exit-error eval_error)))

(if (and typeof_test1 typeof_test2 typeof_test3 typeof_test4 typeof_test5 typeof_test6 typeof_test7 typeof_test8
         is_list_test1 is_list_test2 is_list_test3 is_list_test4 is_list_test5 is_list_test6 is_list_test7 is_list_test8 is_list_test9
         is_number_test1 is_number_test2 is_number_test3 is_number_test4 is_number_test5 is_number_test6 is_number_test7 is_number_test8 is_number_test9 is_number_test10
         to_i_test1 to_i_test2 to_i_test3 to_i_test4 to_i_test5 to_i_test6 to_i_test7 to_i_test8 to_i_test9 to_i_test10
         to_float_test1 to_float_test2 to_float_test3 to_float_test4 to_float_test5 to_float_test6 to_float_test7 to_float_test8 to_float_test9 to_float_test10
         sym_to_str_test1 sym_to_str_test2 sym_to_str_test3 sym_to_str_test4 sym_to_str_test5 sym_to_str_test6 sym_to_str_test7
         str_to_sym_test1 str_to_sym_test2 str_to_sym_test3 str_to_sym_test4 str_to_sym_test5  str_to_sym_test7
         ;;str_to_sym_test6
         )
    (print "SUCCESS")
    (print "FAILURE"))
