(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(define feq (lambda (a b epsilon)
              (< (abs (- a b)) epsilon)))

@const-start

(define pi 3.14159)
(define e 2.71828f64)
(define neg-pi -3.14159)
(define large-float 1234567.89)

(define small-int 42)
(define large-int 1000000)
(define neg-int -123)

(define hello "hello")
(define world "world")
(define empty-str "")

(define test-list '(1 2 3))
(define nested-list '((a b) (c d)))

@const-end

; Test arithmetic operations with boxed constants
(define arith-tests
  (and
    ; Basic arithmetic
    (feq (+ pi e) 5.85987 0.00001)
    (feq (- pi e) 0.42331 0.00001)
    (feq (* pi 2) 6.28318 0.00001)
    (feq (/ pi 2) 1.570795 0.00001)
    (= (mod large-int 7) 1)
    (= (// large-float 1) 1234567)
    
    ; Mixed operations
    (= (+ small-int large-int) 1000042)
    (= (* neg-int small-int) -5166)
    
    ; Single argument cases
    (feq (+ pi) pi 0.00001)
    (feq (- neg-pi) pi 0.00001)
    (feq (* e) e 0.00001)
  ))

; Test comparison operations with boxed constants  
(define comp-tests
  (and
    ; Numerical comparisons
    (= pi pi)
    (= large-int large-int)
    (!= pi e)
    (> pi e)
    (< e pi)
    (>= pi pi)
    (<= e e)
    
    ; Mixed type comparisons
    (> large-float small-int)
    (< neg-int small-int)
    
    ; Single argument cases
    (= pi)
    (<= e)
  ))

; Test type operations with boxed constants
(define type-tests
  (and
    ; Type checking
    (number? pi)
    (number? large-int)
    (list? test-list)
    (eq (type-of pi) 'type-float)
    (eq (type-of e) 'type-double)
    (eq (type-of small-int) 'type-i)
    (eq (type-of hello) 'type-array)
    (eq (type-of test-list) 'type-list)
    
    ; Type conversions
    (= (to-i pi) 3)
    (feq (to-float large-int) 1000000.0f32 0.1)
  ))

; Test list operations with boxed constants
(define list-tests
  (and
    ; Basic list operations
    (= (length test-list) 3)
    (= (car test-list) 1)
    (eq (cdr test-list) '(2 3))
    (= (ix test-list 1) 2)
    
    ; List construction with constants
    (eq (cons small-int test-list) '(42 1 2 3))
    (eq (append test-list '(4)) '(1 2 3 4))
    
    ; Nested list access
    (eq (car (car nested-list)) 'a)
    (eq (cdr (car nested-list)) '(b))
  ))

; Test string and conversion operations with boxed constants
(define string-tests
  (and
    ; String operations
    (eq (str-from-n pi "%.2f") "3.14")
    (eq (str-from-n large-int) "1000000")
    (eq (str-from-n neg-int) "-123")
    
    ; Symbol conversions
    (eq (sym2str 'test) "test")
    (eq (str2sym hello) 'hello)
    
    ; String length and manipulation
    (= (length hello) 6) ;; length is not strlen
    (= (length empty-str) 1) ;; length is not strlen
  ))

; Test boolean operations with boxed constants
(define bool-tests
  (and
    ; Logical operations
    (and (> pi 3) (< pi 4))
    (or (< pi 0) (> pi 0))
    (not (= pi e))
    
    ; Short-circuit evaluation
    (and pi e large-int)  ; All truthy
    (or empty-str pi)     ; First falsy, second truthy
  ))

; Test bitwise operations with boxed integer constants
(define bit-tests
  (and
    (= (shl small-int 1) 84)
    (= (shr large-int 1) 500000)
    (= (bitwise-and large-int 255) 64)
    (= (bitwise-or small-int 128) 170)
    (= (bitwise-xor small-int large-int) 1000042)
    (= (bitwise-not small-int) -43)
  ))

; Error handling with boxed constants
(define error-tests
  (and
    ; Division by zero
    (eq (car (trap (/ pi 0))) 'exit-error)
    
    ; Type errors
    (eq (car (trap (+ hello world))) 'exit-error)
    (eq (car (trap (car pi))) 'exit-error)
    (eq (car (trap (length pi))) 'exit-error)
    
    ; Index out of bounds
    (eq (ix test-list 10) nil)
  ))

(debug_test arith-tests 1)
(debug_test comp-tests 2)
(debug_test type-tests 3)
(debug_test list-tests 4)
(debug_test string-tests 5)
(debug_test bool-tests 6)
(debug_test bit-tests 7)
(debug_test error-tests 8)

(if (and arith-tests comp-tests type-tests list-tests 
         string-tests bool-tests bit-tests error-tests)
    (print "SUCCESS")
    (print "FAILURE"))
