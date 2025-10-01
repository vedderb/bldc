
;; =============================================================================
;; MicroKanren: A Minimal Logic Programming System
;; =============================================================================
;; This is an implementation of microKanren in LispBM, based on the paper
;; "μKanren: A Minimal Functional Core for Relational Programming" by
;; Jason Hemann and Daniel P. Friedman.
;;
;; Key concepts:
;; - Logic variables: Represented as arrays containing a unique counter
;; - Substitutions: Association lists mapping variables to values
;; - States: Pairs of (substitution . counter) tracking the search state
;; - Goals: Functions that take a state and return a stream of states
;; - Streams: Lists of states representing multiple solution paths

;; =============================================================================
;; UTILITY FUNCTIONS
;; =============================================================================

;; Find first association in alist where predicate p returns true for the key
(defun assp (p al)
  (if (eq al nil) nil
    (if (p (car (car al)))
        (car al)                ; return the match
        (assp p (cdr al)))))    ; continue searching

;; Test if value is a pair (cons cell)
(defun pair? (a)
  (match a ((_ . _) t)
           (_ nil)))

;; =============================================================================
;; LOGIC VARIABLES
;; =============================================================================
;; Logic variables are represented as single-element arrays containing
;; a unique integer counter. This allows them to be distinguished from
;; regular values and compared for equality.

;; Create a new logic variable with given counter
(defun kan-var (c) (list-to-array (list c)))

;; Test if value is a logic variable
(defun kan-var? (x) (array? x))

;; Test if two logic variables are the same
(defun kan-var=? (x1 x2) (= (ix x1 0) (ix x2 0)))

;; =============================================================================
;; SUBSTITUTION AND UNIFICATION
;; =============================================================================
;; The core of microKanren is unification - the process of making two terms
;; equal by finding appropriate variable bindings.

;; Ultimate value of U
;; 1: U is not a value -> U is ultimate
;; 2: U is unbound variable -> U is ultimate (the variable itself)
;; 3: U is bound to V, V is bound to .... is bound to X (chain leading to either 1. or 2.) X is ultimate value of U.

;; Follow variable bindings in substitution to find the ultimate value
;; This is called "walking" the substitution chain
(defun walk (u s)
  (let ((pr (and (kan-var? u) (assp (lambda (v) (kan-var=? u v)) (ix s 0)))))
    (if pr (walk (cdr pr) s) u)))

;; Extend substitution with new variable binding x -> v
;; Returns new substitution as a wrapped array
(defun extend-s (x v s)
  (let ((new-list `((,x . ,v) . ,(ix s 0))))
    (list-to-array (list new-list))))

;; =============================================================================
;; GOALS AND GOAL CONSTRUCTORS
;; =============================================================================
;; Goals are functions that take a state and return a stream of states.
;; The unification goal (==) attempts to make two terms equal.

;; Create a unification goal that tries to make u and v equal
(defun kan== (u v)
  (lambda (sc)
    (let ((s (unify u v (car sc))))
      (if s (unit `(,s . ,(cdr sc))) mzero))))

;; Create a singleton stream containing one state
(defun unit (s/c) (cons s/c mzero))

;; The empty stream (represents failure)
(define mzero '())

;; Unify two terms u and v given substitution s
;; Returns new substitution if unification succeeds, nil if it fails
(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ;; Same variable - unification succeeds with current substitution
      ((and (kan-var? u) (kan-var? v) (kan-var=? u v)) s)
      ;; u is unbound variable - bind it to v
      ((kan-var? u) (extend-s u v s))
      ;; v is unbound variable - bind it to u
      ((kan-var? v) (extend-s v u s))
      ;; Both are pairs - unify components recursively
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      ;; Neither is variable - succeed only if they're equal
      (t (and (eq u v) s)))))

;; Introduce a fresh logic variable and apply function f to it
;; This increments the variable counter to ensure uniqueness
(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (kan-var c)) `(,(car s/c) . ,(+ c 1))))))

;; =============================================================================
;; GOAL COMBINATORS
;; =============================================================================
;; These combine multiple goals to create more complex logical relationships

;; Disjunction (OR): Goal succeeds if either g1 OR g2 succeeds
;; Returns a stream containing solutions from both goals
(defun disj (g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))

;; Conjunction (AND): Goal succeeds only if both g1 AND g2 succeed
;; Applies g2 to each solution produced by g1
(defun conj (g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

;; =============================================================================
;; STREAM PROCESSING
;; =============================================================================
;; Streams represent multiple solution paths. They can be:
;; - Empty lists (no solutions)
;; - Lists of states (finite solutions)
;; - Closures (delayed/infinite streams)

;; Merge two streams, ensuring fair interleaving of solutions
;; This prevents one infinite stream from blocking another
(defun mplus (a b)
  (match a
         (nil b)  ; First stream empty - return second
         ;; First stream is delayed - swap and delay the merge
         ((closure _ _ _ _) (lambda () (mplus b (a))))
         ;; First stream has solutions - take one and merge rest with b
         (_ (cons (car a) (mplus (cdr a) b)))))

;; Apply goal g to each state in stream a
;; This is the monadic bind operation for the stream monad
(defun bind (a g)
  (match a
         (nil mzero)  ; Empty stream - return empty
         ;; Delayed stream - delay the bind operation
         ((closure _ _ _ _) (lambda () (bind (a) g)))
         ;; Stream with solutions - apply g to first, bind to rest
         (_ (mplus (g (car a)) (bind (cdr a) g)))))


;; =============================================================================
;; INITIAL STATE
;; =============================================================================

;; Empty substitution (wrapped in array to be truthy)
(define empty-sub [| () |])

;; Initial state: empty substitution with variable counter 0
(define empty-state `(,empty-sub . 0))

;; =============================================================================
;; EXAMPLE PROGRAMS
;; =============================================================================

;; Example 1: Simple variable unification
;; This shows how to unify a fresh variable with the value 5
;; Result would be: '(((#(0) . 5)) . 1)
;; (let ((a ((call/fresh (lambda (q) (kan== q 5))) empty-state)))
;;   (car a))

;; Example 2: Conjunction with disjunction
;; Create variable 'a' that equals 7, AND variable 'b' that equals either 5 OR 6
(define a-and-b
  (conj
   (call/fresh (lambda (a) (kan== a 7)))
   (call/fresh
    (lambda (b)
      (disj
       (kan== b 5)
       (kan== b 6))))))

;; Example 3: Building a sentence "I love you"
;; Shows how to construct complex terms using fresh variables
(define love
  (call/fresh (lambda (res)
                (call/fresh (lambda (a)
                              (call/fresh (lambda (b)
                                            (call/fresh (lambda (c)
                                                          (conj (kan== a 'i)
                                                                (conj
                                                                 (kan== b 'love)
                                                                 (conj
                                                                  (kan== c 'you)
                                                                  (kan== res (list a b c))))))))))))))

;; Utility function to substitute variables in a list with their values
(define var-subst (lambda (ls as)
                    (if (eq nil ls ) nil
                      (let (( a (car ls))
                            ( b (assoc as a)))
                        (if b (cons b (var-subst (cdr ls) as))
                          (cons a (var-subst (cdr ls) as)))))))

;; Execute the love relation and extract the result
;; This demonstrates how to run a relation and process the results
(cdr (let ((res (love empty-state)))
       (let ((subst (ix (car (car res)) 0)))
         (var-subst subst subst))))


;; =============================================================================
;; MATHEMATICAL RELATIONS
;; =============================================================================
;; These demonstrate how to encode mathematical concepts as logical relations

;; Natural number relation using Peano arithmetic
;; A number is either 'zero or 'succ of another natural number
;; This creates an infinite relation that can generate all natural numbers
(define nato
    (lambda (x)
      (disj
       ;; Base case: zero is a natural number
       (kan== x 'zero)
       ;; Recursive case: succ(n) is natural if n is natural
       (call/fresh (lambda (n)
                     (conj
                      (kan== x `(succ ,n))
                      (nato n)))))))

;; Addition relation: pluso(X, Y, Z) means X + Y = Z
;; Encodes the recursive definition:
;; - 0 + Y = Y (base case)
;; - succ(X) + Y = succ(Z) if X + Y = Z (recursive case)
(define pluso
    (lambda (x y z)
      (disj
       ;; Base case: zero + Y = Y
       (conj (kan== x 'zero) (kan== y z))
       ;; Recursive case: succ(X) + Y = succ(Z) if X + Y = Z
       (call/fresh (lambda (x1)
                     (call/fresh (lambda (z1)
                                   (conj
                                    (kan== x `(succ ,x1))
                                    (conj
                                     (kan== z `(succ ,z1))
                                     (pluso x1 y z1))))))))))

;; =============================================================================
;; TEST CASES AND DEMONSTRATIONS
;; =============================================================================

;; Define some concrete numbers in Peano arithmetic
(define two '(succ (succ zero)))
(define three '(succ (succ (succ zero))))
(define five '(succ (succ (succ (succ (succ zero))))))

;; Test: Verify that 2 + 3 = 5
(define verify-addition
    (pluso two three five))

(print "Proving 2 + 3 = 5:")
(let ((result (verify-addition empty-state)))
  (if (eq result mzero)
      (print "FAILED")
      (print "SUCCESS: 2 + 3 = 5 is proven!")))

;; Test: Try a false statement to ensure the system rejects incorrect math
(define seven '(succ (succ (succ (succ (succ (succ (succ zero))))))))
(define false-addition (pluso two three seven))

(print "Testing false statement: 2 + 3 = 7:")
(let ((result (false-addition empty-state)))
  (if (eq result mzero)
      (print "CORRECTLY FAILED: 2 + 3 != 7")
      (print "ERROR: False statement succeeded!")))

;; Test: Basic unification sanity check
(print "Testing simple unification (5 = 5):")
(let ((result ((kan== 5 5) empty-state)))
  (if (eq result mzero)
      (print "FAILED")
      (print "SUCCESS: Simple unification works")))

;; Test: Verify the base case of addition (0 + 0 = 0)
(print "Testing 0 + 0 = 0:")
(let ((result ((pluso 'zero 'zero 'zero) empty-state)))
  (if (eq result mzero)
      (print "FAILED")
      (print "SUCCESS: 0 + 0 = 0 proven!")))
