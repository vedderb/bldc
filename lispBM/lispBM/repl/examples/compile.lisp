
;; Lbm compiler experiment

;; Bytecode format
;; Lisp array containing:
;;  1. number of arguments
;;  2. byte-array with byte-code
;;  3. lisp array of "constants"

;; IR format
;; List containing:
;; 1. list of arguments - for index backwards lookup ?
;; 2. list of opcode/arguments
;; 3. list of constants

;; Not sure we need arguments. argument references could be done with an index.
;; - What about things like rest-args..
;;

;; closure application gets an environment populted with param/value pairs for
;; the arguments. For compiled code, it may be easier to push the param values
;; to the stack and then generate code with stack-index operations where argument
;; accesses take place.

;; NOTE:
;; - tailcalls, detect and enable ?
;; - recursive compiled functions ? Since we currently compile lambdas, they dont even have names.

;; Registers needed
;; r, pc

;; TODO: maintain scopes on a stack of (variable . stackpos) association lists.
;; Things that add a scope should push a alist onto the stack that is popped when
;; the compiler exits this scope.
;; Variables should be search forin TOS-alist first then downwards the stack.
;; code generated for leaving a scope will discard N elements from the stack.
;; The N is determined by size of the scope popped.

(def lbm-opcodes
     '(stack-ref
       constant
       call-fundamental
       call-extension
       push-r
       pop-r
       end       ;; terminate the byte-code evaluator
       discard-n ;; remove N elements from the stack
       ))

(defun mkir ()
  (let ((ir (mkarray 2)))
    (setix ir 0 nil)
    (setix ir 1 nil)))

(defun is-symbol (e)
  (eq (type-of e) type-symbol))

(defun is-list (e)
  (eq (type-of e) type-list))

(defun is-symbol (e)
  (eq (type-of e) type-symbol))

(defun is-number (e)
  (let ((type (type-of e)))
    (or (eq type type-char) ;; symbol 91
        (eq type type-byte) ;; symbol 92  This is not that good.
        (eq type type-i)
        (eq type type-u))))

(defun is-zero (e)
  (if (is-number e)
      (= 0 e)
    nil))

(defun is-not-zero (e)
  (not (is-zero e)))

(def fundamentals
     (list '+
           '-
           '*
           '/
           'mod
           '=
           'length
           )) ;; to start with

(def special-forms
     (list 'define
           'let));; to start with

(defun is-fundamental (f)
  (member fundamentals f))

(defun is-special-form (f)
  (member special-forms f))

(defun is-closure (f)
  (eq (ix f 0) 'closure))

(defun filter (f xs)
  (match xs
         ( ((? x) . (? xs)) (if (f x) (cons x (filter f xs))
                              (filter f xs)))
         ( nil nil)))

(defun index-lookup (x xs)
  (let ( (find-ix (lambda (x xs inx)
                    (match xs
                           ( ((? y) . (? ys)) (eq y x) inx )
                           ( ((? y) . (? ys)) (find-ix x ys (+ inx 1)))
                           ( _ 'nil)))))
    (find-ix x xs 0)))

(defun compile-list (ps const env xs)
  (match xs
         ( ((? x) . (? xs))
           {
           (var (ps-new const-new code) (compile ps const env x))
           (var (ps-fin const-fin code-fin) (compile-list ps-new const-new env xs))
           (list ps-fin const-fin (append code code-fin))
           }
           )
         ( _ (list ps const nil))))

;; Code specific to associative operator with 0 identity.
;; TODO: generalize
(defun compile-fundamental (ps const env x xs)
  {
  (print "compiling fundamental " x)
  (var constants (filter (lambda (x) (is-number x)) xs))
  (var unknown   (filter (lambda (x) (not (is-number x))) xs))
  (if constants
      {
    (var new-e (eval (cons x constants)))
    (var new-xs (cons new-e unknown))
    (var xs-1 (filter is-not-zero new-xs))
    (var (ps-new const-new code) (compile-list ps const env xs-1))
    (list ps-new const-new (append code (list (list 'call-fundamental x (length xs-1))
                                              'push-r)))
    }
    {
    (var (ps-new const-new code) (compile-list ps const env unknown))
    (list ps-new const-new (append code (list (list 'call-fundamental x (length unknown))
                                              'push-r)))
    }
   )
  })

;; Extensions are similar to fundamentals.
;; All arguments compiled and should go onto stack
;; then opcode call-extension should take care of the rest.

;; Example compile define.
;;
;; (compile  (define x something))
;; push x
;; something-code   (probably ends in push-r, so a peephole opt can remove)
;; pop-r
;; call-continuation set_global_env

(defun compile-special-form (ps const env x xs)
  (print "special form"))

(defun gen-constant (ps const e)
  (let (( inx (index-lookup e const))
        ( n   (length const)))
    (if inx
        (list ps const `((constant ,(- n inx 1))))
      (list ps (cons e const) `((constant ,n))))))

;; Stack-ref currently not correct.
;;

(defun compile (ps const env body)
  (match body
         ( ((? x) . (? xs)) (cond ((is-fundamental x) (compile-fundamental ps const env x xs))
                                  ((is-special-form x) (compile-special-form ps const env x xs))))
         ( (? x) (cond ((is-symbol x) (let (( inx (index-lookup x ps)))
                                        (if inx
                                            (list ps const `((stack-ref ,inx)))
                                          (if (assoc env x)
                                              (gen-constant ps const (assoc env x))
                                            (print "Cannot compile " x)))))
                       ((is-number x) (gen-constant ps const x))
                       (t             (list ps const nil))
                       ))))

(defun peep-opt (x)
  (match x
         ( (push-r pop-r . (? xs)) (peep-opt xs))
         ( ((? x) . (? xs)) (cons x (peep-opt xs)))
         (nil nil)))

(defun compile-fun (e)
  (match e
         ( (closure (? ps) (? body) (? env))
           {
           (var (ps-fin const-fin code-fin) (compile ps nil env body))
           (list ps-fin const-fin (peep-opt (append code-fin (list 'pop-r 'end))))
           }
           )
         ( (lambda (? ps) (? body))
           {
           (var (ps-fin const-fin code-fin) (compile ps nil nil body))
           (list ps-fin const-fin (peep-opt (append code-fin (list 'pop-r 'end))))
           }
           )
         ( _ (print "Can only compile lambdas/closures"))
         ))

(compile-fun '(apa))

(compile-fun '((f 1 2 3)))

(let ((y 10))
  (compile-fun (lambda (x) (+ x y)))
  )

(define f (lambda (x) (+ x 1)))

(define f-c (compile-fun f))

(compile-fun (lambda (xs) (length xs)))

