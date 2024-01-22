# LispBM language reference

## About Symbols

Symbols are very important and central to LispBM and also perhaps
a bit different from identifiers/names used in languages such as C, so
a short intro on symbols could be good here.

A symbol can be thought of as a name and can be used to give names
to functions or values (variables). A symbol can also be treated and
used as a value in and of itself a value (or data). So it can be used
to name data and functions and is itself also data.

---

**NOTE**

Symbols are expressed as strings in your program such as `a`, `let`,
`define`, `+` or `orange`. The "reader", the part of LBM that parses
code, translates each symbol into a 28bit value. The string `orange`
for example is only of interest if you print a symbol and then the
runtime system will look up what string corresponds to the 28bit
identifier you want to print. So the runtime system is never wasting
time comparing strings to see if a symbol is this or that symbol, it's
all integer comparisons.

---

You associate values with symbols using, <a href="#define">define</a>,
<a href="#let">let</a> and you can change the value bound to a "variable"
using <a href="#set">set</a>, <a href="#setvar">setq</a> or <a href="#setvar">setvar</a>.

Not all symbols are treated the same in LBM. Some symbols are treated as
special because of their very fundamental nature. Among these special symbols
you find `define`, `let` and `lambda` for example. These are things that you
should not be able to redefine and trying to redefine them leads to an error.
Symbols that start with `ext-` are special and reserved for use together
with extensions that are loaded and bound at runtime.

Examples of symbols used as data are `nil` and `t`. `nil` representds
"nothing", the empty list or other similar things and `t`
represents true.  But any symbol can be used as data by quoting it
`'`, see <a href="#quotes-and-quasiquotation"> Quotes and
Quasiquotation </a>.

### Valid symbol names

A symbol is a string of characters following the rules:
1. The first character is a one of 'a' - 'z' or 'A' - 'Z' or '+-*/=<>#!'.
2. The rest of the characters are in 'a' - 'z' or 'A' - 'Z' or '0' - '9' or '+-*/=<>!?_'.
3. At most 256 characters long.

Note that lower-case and upper-case alphabetical letters are considered identical
so the symbol `apa` is the same symbol as `APA`.

examples of valid symbols
```
apa
apa?
!apa
kurt_russel_is_great
```



## Arithmetic

### +

Adds up an aribtrary number of values. The form of a `+` expression is
`(+ expr1 ... exprN)`

Example adding up two numbers. The result is 3.
```clj
(+ 1 2)
```
When adding up values of different types values are converted.
 ```clj
(+ 1i 3.14)
```
The example above evaluates to float value 4.14.<br>
You can add up multiple values.
```clj
(+ 1 2 3 4 5 6 7 8 9 10)
```
The example above results in the value 55.

---

### -

Subtract an arbitrary number of values from a value. The form of a -
expression is `(- expr1 ... exprN)`

Example subtracting 3 from 5.
```clj
(- 5 3)
```

---

### *

Multiplying an arbitrary number of values. The form of a * expression
is `(* expr1 ... exprN)`


Example 2pi.
```clj
(* 2 3.14)
```


---

### /

Division. The form of a / expression is `(/ expr1 ... exprN)`.

Divide 128 by 2
```clj
(/ 128 2)
```
The following example evaluates to 1.
```clj
(/ 128 2 2 2 2 2 2 2)
```

---

### mod

Modulo operation. The form of a mod expression is `(mod expr1 ... exprN)`.


Compute 5 % 3, evaluates to 2.
```clj
(mod 5 3)
```


---

## Comparisons


### eq

Compare expressions for equality. The `eq` operation implements structural
equality.  The form of an `eq` expression is `(eq expr1 ... exprN)`


Compare the result of `(+ 1 2)` with 3. The
result of this comparison is `t`.
```clj
(eq (+ 1 2) 3)
```
Multiple expressions can be checked at once. The examples below evaluates to
`t`
```clj
(eq 1 1 1 1)

(eq (+ 3 4) (+ 2 5) (+ 1 6))
```
The following examples evaluate to `nil` representing false.
```clj
(eq 1 1 1 1 2)

(eq (+ 1 2) (+ 0 2) (+ -1 2))
```

The `eq` comparison can be used on tree shaped data. The following
expression evaluates to `t`.

```clj
(eq '(1 (1 2)) '(1 (1 2)))
```

---

### not-eq

`not-eq` implements the negation of eq. In other words, `(not-eq a b c)` evaluates
to the same result as `(not (eq a b c))`.

---

### =

The `=` operation can only be used on numerical arguments.
If you know you are comparing numbers, it will be more efficient to use
`=`.

An important difference between `eq` and `=` is
that `=` compare the numerical values of the arguments. A 3 is a 3
independent of them being different types. `eq` on the other
hand compares the representations of the arguments exactly and they must
match in structure, type and value to be considered equal.


Example of `=` comparison.
```clj
(= (+ 2 3) (+ 1 4))
```


---

### !=

The `!=` operation implements the negation of `=`. So, `(!= a b)` evaluates
to the same result as `(not (= a b))`.

---

### >

Greater than comparison. A greater than comparison has the form `(> expr1 ... exprN)`
and evaluates to `t` if expr1 is greater than all of expr2 ... exprN.


Example
```clj
(> 5 2)
```

---

### <

Less than comparison. A less than comparison has the form `(> expr1 ... exprN)`
and evaluates to `t` if expr1 is less than all of expr2 ... exprN.


Example
```clj
(< 5 2)
```

---

### >=

Greater than or equal comparison. A greater than comparison has the form `(>= expr1 ... exprN)`
and evaluates to `t` if expr1 is greater than or equal to all of expr2 ... exprN.


Example
```clj
(>= 5 2)
```

---

### <=

Less than or equal comparison. A less than or equal comparison has the form `(<= expr1 ... exprN)`
and evaluates to `t` if expr1 is less than or equal to all of expr2 ... exprN.


Example
```clj
(<= 5 2)
```





---

## Boolean operators

### and

Boolean `and` operation between n arguments. The form
of an `and` expression is `(and expr1 ... exprN)`.
This operation treats all non-nil values as true. Boolean `and`
is "shirt-circuiting" and only evaluates until a false is encountered.


The example below evaluates to `t`
```clj
(and t t)
```
The following example evaluates to 3
```clj
(and t t (+ 1 2))
```
And lastly an example that evaluates to nil (for false).
```clj
(and t (< 5 3))
```


---

### or

Boolean `or` operation between n arguments. The form
of an `or` expression is `(or expr1 ... exprN)`.
This operation treats all non-nil values as true. Boolean `or`
is "short-circuiting" and only evaluates until a true is encountered.


The example below evaluates to `t`.
```clj
(or t nil)
```


---

### not

Boolean `not` takes one argument. The form of a `not`
expression is `(not expr)`. All non-nil values are considered
true.

The following example evaluates to `t`
```clj
(not nil)
```


---


## Bit level operations

### shl

The shift left operation takes two arguments. The first argument is a value to shift and the
second argument is the number of bit positions to shift the value.

The example below evaluates to 4.
```clj
(shl 1 2)
```

---

### shr

The shift right operation takes two arguments. The first argument is a value to shift and the
second argument in the number of bit positions to shift the value.

The example below evaluates to 1.
```clj
(shr 4 2)
```

---

### bitwise-and

Performs the bitwise and operation between two values. The type of the result
is the same type as the first of the arguments.

---


### bitwise-or

Performs the bitwise or operation between two values. The type of the result
is the same type as the first of the arguments.

---

### bitwise-xor

Performs the bitwise xor operation between two values. The type of the result
is the same type as the first of the arguments.

---

### bitwise-not

Performs the bitwise not operations on a value. The result is of same type as
the argument.

---

## nil and t, true and false

### nil

Represents the empty list. The nil value is also considered to be false by
conditionals.

The example below creates a one element list by allocating a cons cell and putting a value (1) in the <a href="#car"> car </a> field
and nil in the <a href="#cdr"> cdr </a> field.
```clj
(cons 1 nil)
```

---

### t

All non nil values are considered true in conditionals. `t` should be used in cases where an
explicit true makes sense.

---

### true

`true` is an alias for `t`.

---

### false

`false` is an alias for `nil`.

---

## Quotes and Quasiquotation

Code and data share the same representation, it is only a matter of how
you look at it. The tools for changing view, or interpretation,  are the quotation and
quasiquotation operations.

---

### quote

Usages of the `'` quote symbol in input code is replaced with the symbol quote
by the reader.
Evaluating a quoted expression, (quote a), results in a unevaluated.

The program string `'(+ 1 2) ` gets read into the heap as the list `(quote (+ 1 2))`.
Evaluating the expression `(quote (+ 1 2))` results in the value `(+ 1 2)`.

---

### `

The backwards tick `` ` `` is called the quasiquote. It is similar to
the `'` but allows splicing in results of computations using the <a
href="#,">,</a> and the <a href="#commaat">,\@</a> operators.


The result of `'(+ 1 2)` and `` `(+ 1 2)`` are similar in effect. Both
result in the result value of `(+ 1 2)`, that is a list containing +,
1 and 2.  When `` `(+ 1 2)`` is read into the heap it is expanded into
the expression `(append (quote (+)) (append (quote (1)) (append (quote
(2)) (quote nil))))` which evaluates to the list `(+ 1 2)`.


---

### ,

The comma is used to splice the result of a computation into a quasiquotation.

The expression `` `(+ 1 ,(+ 1 1))`` is expanded by the reader into
`(append (quote (+)) (append (quote (1)) (append (list (+ 1 1)) (quote nil))))`.
Evaluating the expression above results in the list `(+ 1 2)`.

---

### ,@

The comma-at operation is used to splice in the result of a computation (that
returns a list) into a list.

Example:
```clj
(define mylist (list 1 2 3 4 5)
`(9 6 5 ,@mylist)
```
Evaluates to the list `(9 6 5 1 2 3 4 5)`.


## Built-in operations

### eval

Evaluate data as an expression. The data must represent a valid expression.

Example that evaluates to 3.
```clj
(eval (list + 1 2))
```

---

### eval-program

Evaluate a list of data where each element represents an expression.

Example that results in the value 15:
```clj
(define prg '( (+ 1 2) (+ 3 4) (+ 10 5)))
(eval-program prg)
```

Example that prints the strings "apa", "bepa" and "cepa":
```clj
(define prg '( (print "apa") (print "bepa") (print "cepa")))
(eval-program prg)
```


---

### type-of

The `type-of` function returns a symbol that indicates what type the
argument is. The form of a `type-of` expression is `(type-of expr)`.


Example that evaluates to `type-float`.
```clj
(type-of 3.14)
```

---

### sym2str

The `sym2str` function converts a symbol to its string representation.
The resulting string is a copy of the original so you cannot destroy built in symbols using
this function.


Example that returns the string `"lambda"`.
```clj
(sym2str 'lambda)
```

---

### str2sym

The `str2sym` function converts a string to a symbol.

Example that returns the symbol `hello`.
```clj
(str2sym "hello")
```

---

### sym2u

The `sym2u` function returns the numerical value used by the runtime system
for a symbol.


Example that evaluates to 4.
```clj
(sym2u 'lambda)
```

---

### u2sym

The `u2sym` function returns the symbol associated with the
numerical value provided. This symbol may be undefined in which case you
get as result a unnamed symbol.

---

## Special forms


### if

Conditionals are written as `(if cond-expr then-expr else-expr)`.
If the cond-expr evaluates to <a href="#nil"> nil </a> the else-expr will be evaluated.
for any other value of cond-expr the then-expr will be evaluated.


The example below evaluates to 0 if a is less than or equal to 4. Otherwise it evaluates to a + 10.
```clj
(if (> a 4) (+ a 10) 0)
```

---

### cond

`cond` is a generalization of `if` to discern between n different cases
based on boolean expressions. The form of a `cond` expression is:
`(cond ( cond-expr1 expr1) (cond-expr2 expr2) ... (cond-exprN exprN))`.
The conditions are checked from first to last and for the first `cond-exprN`
that evaluates to true, the corresponding `exprN` is evaluated.

If no `cond-exprN` evaluates to true, the result of the entire conditional
is `nil`.

Example that prints "Hello world":
```clj
(define a 0)

(cond ( (< a 0) (print "abrakadabra"))
      ( (> a 0) (print "llama"))
      ( (= a 0) (print "Hello world")))
```

Example that evaluates to `nil` as none of the conditions evaluate to true.
```clj
(define a 5)

(cond ( (= a 1)  'doughnut )
      ( (= a 7)  'apple-strudel )
      ( (= a 10) 'baklava))
```
---

### lambda

You create an anonymous function with lambda. The function can be given a name by binding the lambda expression using <a href="#define">define</a>
or <a href="#let">let</a>. A lambda expression has the form `(lambda param-list body-expr)`.

The example shows an anonymous function that adds one.
```clj
(lambda (x) (+ x 1))
```
A lambda can be immediately applied to an argument.
```clj
((lambda (x) (+ x 1)) 10)
```
The application above results in the value 11.
Using <a href="#define"> define </a> you can give a name to the function.
```clj
(define inc (lambda (x) (+ x 1)))
```
Now the expression `(inc 10)` computes the result 11.

---

### closure

A <a href="#lambda"> lambda </a> expression evaluates into a closure which is very similar to a <a href="#lambda">lambda</a>
but extended with a captured environment for any names unbound in the param-list appearing in the body-expr.
The form of a closure is `(closure param-list body-exp environment)`.

Evaluation of the expression
```clj
(lambda (x) (+ x 1))
```
results in the value
```clj
(closure (x) (+ x 1) nil)
```
Below is an example of how a value is captured into the closure.
```clj
(let ((a 1)) (lambda (x) (+ x a)))
```
The expression above evaluates to the following. Note that `(a . 1)` appears in
the closure.
```clj
(closure (x) (+ x a) ((a . 1)))
```

---

### let

Local environments are created using let. The let binding in
lispbm allows for mutually recursive bindings. The form of a let is
`(let list-of-bindings body-expr)` and evaluating this expression
means that body-expr is evaluted in an environment extended with the list-of-bindings.

Example that evaluates to 3.
```clj
(let ((a 1)
      (b 2))
  (+ a b))
```
Below is a more advanced example of two mutually recursive functions created
in a let binding.
```clj
(let ((f (lambda (x) (if (= x 0) 0 (g (- x 1)))))
      (g (lambda (x) (if (= x 0) 1 (f (- x 1))))))
  (f 11))
```
The mutually recursive program above evaluates to 1.

---

Let supports deconstructive bindings. These are bindings that decompose a complex
value into constituents.

Example:

```clj
(let (( ( a . b) '(1 . 2) ))
   (+ a b))
```

In the example, the bindings a = 1 and b = 2 are created for use in the let body.

### loop
loop allows to repeatedly evaluate an expression for as long as a condition
holds. The form of a loop is `(loop list-of-local-bindings condition-exp body-exp)`.

The  `list-of-local-bindings` are very similar to how `let` works, just that here
the `body-exp` is repeated.

Example that prints hello world 5 times:
```clj
(loop ( (a 5) ) (> a 0) { (print "hello world") (setq a (- a 1))})
```

When the `cond-exp` evaluates to false, the loop exits and the entire loop
expression returns the value `nil`.

Here is an example of a "for each element in a list do something":
```clj
(loop ( (a '(1 2 3 4)) ) a { (print (car a)) (setq a (cdr a))})
```

The example above loops for as long as there are elements in a. Each
element is printed and a is updated in the body.


### define

You can give names to values in a global scope by using define.
The form of define is `(define name expr)`. The expr is evaluated and it is the
result of the evaluated expr that is stored in the environment.
In lispbm you can redefine already defined values.

Example
```clj
(define apa 10)
```
---

### undefine

A definition in the global can be removed using undefine.  The form of
an undefine expression is `(undefine name-expr)` where name-expr
should evaluate to a symbol (for example `'apa`).

Example
```clj
(undefine 'apa)
```

It is also possible to undefine several bindings at the same time by
providing a list of names.

Example
```clj
(undefine '(apa bepa cepa))
```

---

### setvar

The `setvar` form is used to change the value of some variable in an environment.
You can use `setvar` to change the value of a global definition or a local definition.
An application of the `setvar` form looks like `(setvar var-expr val-expr)` where
`var-expr` should evaluate to a symbol. The `val-expr` is evaluated before
rebinding the variable. `setvar` returns the value that `val-expr` evaluates to.

Examples:
```clj
(define a 10)
```
The variable `a` is now `10` in the global environment.
```clj
(setvar 'a 20)
```
Now, the value of `a` will be 20. Note that `a` is quoted in the `setvar` form application
while it is not in the `define` form. This is because `define` requires the first
argument to be a symbol while the `setvar` form requires the first argument to evaluate
into a symbol.

You can also set the value of a let bound variable.
```clj
(let ((a 10)) (setvar 'a 20))
```

---

### set

The `set` form is used to change the value of a variable in an environment.
It behaves identical to `setvar`.

Example:

```clj
(define a 10)
```
The variable `a` is now 10 in the global environment.

```clj
(set 'a 20)
```

And now `a` is associated with 20 in the global environment.

`set` works in local environments too such as in the body of a `let`
or in a `progn`-local variable created using `var`.

Example:

```clj
(progn (var a 10) (set 'a 20) a)
```

The expression above evaluates to 20.

### setq

The `setq` special-form is similar to `set` and to `setvar` but expects the first argument
to be a symbol. The first argument to `setq` is NOT evaluated.

Example:
```clj
(define a 10)
```
The variable `a` is now 10 in the global environment.

```clj
(setq a 20)
```

And now `a` has been associated with the value 20 in the global env.

Just like `set` and `setvar`, `setq` can be used on variables that
are bound locally such as in the body of a `let` or a `progn`-local variable
created using `var`.

### progn

The progn special form allows you to sequence a number of expressions.
The form of a progn expression is:
```clj
(progn expr1
       expr2
       ...
       exprN)
```
The evaluation result of a progn sequence is the value that the last `exprN`
evaluated to. This is useful for sequencing of side-effecting operations.

Simple example that evaluates to 3:
```clj
(progn 1
       2
       3)
```
An example where side effects are sequenced:
```clj
(progn (define a 10)
        (define b 20)
        (+ a b))
```
This program evaluates 30 but also extends the global environment with the
2 bindings `(a 10)` and `(b 20)` created using <a href="#define">define</a>.

---

### {

The curlybrace `{` syntax is a short-form (syntactic sugar) for `(progn`.
The parser replaces occurrences of `{` with `(progn`. The `{` should be
closed with an `}`.

These two programs are thus equivalent:

```clj
(progn
  (define a 10)
  (define b 20)
  (+ a b))
```

And

```clj
{
  (define a 10)
  (define b 20)
  (+ a b)
}
```

---

### }

The closing curlybrace `}` should be used to close an opening `{` but purely
for esthetical reasons. The `}` is treated identically to a regular closing parenthesis `)`.

The opening `{` and closing `}` curlybraces are used as a short-form for `progn`-blocks
of sequences expressions.


### var

The var special form allows local bindings in a progn expression. A
var expression is of the form (var symbol expr) and the symbol `symbol`
is bound to the value that `expr` evaluates to withing the rest of the progn expression.

Example:

```clj
(defun f ()
  (progn
    (var a 10)
    (var b 20)
    (+ a b)))
```

and:

```clj
(defun f ()
  (progn
    (var a 10)
    (var b (+ a 10))
    (+ a b)))
```

### read

Parses a string resulting in either an expression or the <a href="#read_error">read_error</a> in case
the string can not be parsed into an expression. The form of a read expression is
`(read string)`.


The example below evaluates to the value 1:
```clj
(read "1")
```
You can also read code:
```clj
(read "(lambda (x) (+ x 1))")
```
That lambda you just read in from a string can be directly applied to an
argument if using an application of eval to evaluate the read lambda into a closure.
```clj
((eval (read "(lambda (x) (+ x 1))")) 10)
```
The code above evaluates to 11.


---

<a name="read-program"> <h3>read-program</h3> </a>

Parses a string containing multiple sequenced expressions. The resulting list of
expressions can be evaluated as a program using <a href="#eval-program">eval-program</a>.
The form of a read-program expression is `(read-program string)`.


Evaluate a program you just read from a string with <a href="#eval-program">eval-program</a>.
```clj
(eval-program (read-program "(define apa 1) (+ 2 apa)"))
```
The expression above evaluates to 3 with the side effect that the global environment
has been extended with the binding `(apa 1)`.

---

### read-eval-program

Parses and evaluates a program incrementally. `read-eval-program` reads a top-level expression
then evaluates it before reading the next.

Example that evaluates to 20:

```clj
(read-eval-program "(define a 10) (+ a 10)")
```

`read-eval-program` supports the `@const-start`, `@const-end` which moved all
global definitions created in the program to constant memory (flash).

Example that evaluates to 20:

```clj
(read-eval-program "@const-start (define a 10) (+ a 10) @const-end")
```
---

## Lists and cons cells

Lists are built using cons cells. A cons cell is represented by the lbm_cons_t struct in the
implementation and consists of two fields named the `car` and the `cdr`.
There is no special meaning associated with the `car` and the `cdr` each can hold
a lbm_value. See <a href="#cons">cons</a> and <a href="#list">list</a> for two ways to create structures of
cons cells on the heap.

![cons cell](images/cons_cell.png?raw=true "cons cell")

A cons cell can be used to store a pair of values. You create a pair by
sticking a value in both the car and cdr field of a cons cell using either `'(1 . 2)` or
`(cons 1 2)`.

![pair](images/pair.png?raw=true "pair")

A list is a number of cons cells linked together where the car fields hold values
and the cdr fields hold pointers (the last cdr field is nil). The list below
can be created either as `'(1 2 3)` or as `(list 1 2 3)`.

![list](images/list.png?raw=true "pair")


### car

Use `car` to access the `car` field of a cons cell. A
`car` expression has the form `(car expr)`.


Taking the `car` of a number of symbol type is in general a <a href="#type_error">type_error</a>.
The following program results in `type_error`.
```clj
(car 1)
```
The next example evaluates to 1.
```clj
(car (cons 1 2))
```
The `car` operation accesses the head element of a list. The following program evaluates to 9.
```clj
(car (list 9 8 7))
```

---

### first

`first` is an alternative (and one that makes some sense) name for the `car` operation.

Use `first` to access the first element of a list or pair. A `first` expression  has the form `(first expr)`.

```clj
# (first (list 1 2 3 4))
> 1
```

---

### cdr

Use `cdr` to access the `cdr` field of a cons cell. A
`cdr` expression has the form `(cdr expr)`.

The example below evaluates to 2.
```clj
(cdr (cons 1 2))
```
The `cdr` operation gives you the rest of a list. The example below evaluates to the list (8 7).
```clj
(cdr (list 9 8 7))
```

---

### rest

`rest` is an alternative name for the `cdr` operation.

Use `rest` to access all elements except the first one of a list, or to access the second element in a pair. A `rest` expression has the form `(rest expr)`.

```clj
# (rest (list 1 2 3 4))
> (2 3 4)
```

---

### cons

The `cons` operation allocates a cons cell from the heap and populates the
`car` and the `cdr` fields of this cell with its two arguments.
The form of a `cons` expression is `(cons expr1 expr2)`.

Build the list `(1 2 3)` using cons. <a href="#nil">nil</a> terminates a proper list. 
```clj
(cons 1 (cons 2 (cons 3 nil)))
```
Construct the pair `(+ . 1)` using cons.
```clj
(cons + 1)
```

---

### .

The dot, `.`, operation creates a pair. The form of a dot expression
is `(expr1 . expr2)`. By default the evaluator will attempt to evaluate the
result of `(expr1 . expr2)` unless it is prefixed with `'`.

Example that creates the pair (1 . 2)
```clj
'(1 . 2)
```

---

### list

The `list` function is used to create proper lists. The function
takes n arguments and is of the form `(list expr1 ... exprN)`.

Example that creates the list (1 2 3 4).
```clj
(list 1 2 3 4)
```

---

### length

Computes the length of a list. The `length` function takes
one argument and is of the form `(length expr)`.

Example that evaluates to 4
```clj

(length (list 1 2 3 4))
```

---

### range

The `range` function computes a list with integer values from a
range specified by its endpoints. The form of a range expression
is `(range start-expr end-expr)`. The end point in the range is excluded.

Example that generates the list (4 5 6 7).
```clj
(range 4 8)
```

A range specified with the end-point being smaller than the
starting point is in descending order.

Example that generates the list (7 6 5 4).
```clj
(range 8 4)
```

Negative number can be used to specify a range

Example that generates the list (-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9)
```clj
(range -10 10)
```

---

### append

The `append` function combines two lists into a longer list.
An `append` expression is of the form `(append expr1 expr2)`.

Example that combines to lists.
```clj
(append (list 1 2 3) (list 4 5 6))
```

---

### ix

Index into a list using the `ix` function. The form of an `ix` expression
is `(ix list-expr index-expr)`. Indexing starts from 0 and if you index out of bounds the result is nil.

Example that evaluates to 2.
```clj
(ix (list 1 2 3) 1)
```

---

### setix

Destructively update an element in a list. The form of a `setix` expression
is `(setix list-expr index-extr value-expr)`. Indexing starts from 0 and
if you index out of bounds the result is nil.

```clj
# (setix (list 1 2 3 4 5) 2 77)
> (1 2 77 4 5)
```

---

### setcar

The `setcar` is a destructive update of the car field
of a cons-cell.

Define `apa` to be the pair `(1 . 2)`
```clj
(define apa '(1 . 2))
```
Now change the value in the car field of apa to 42.
```clj
(setcar apa 42)
```
The `apa` pair is now `(42 . 2)`.

---

### setcdr

The `setcdr` is a destructive update of the cdr field of a cons-cell.


Define `apa` to be the pair `(1 . 2)`
```clj
(define apa '(1 . 2))
```
Now change the value in the cdr field of apa to 42.
```clj
(setcdr apa 42)
```
The `apa` pair is now `(1 . 42)`.

---

### take

`take` creates a list containing the `n` first elements of another list.
The form of a `take` expression is `(take list-exp n-exp)`.

Example that takes 5 elements from a list:
```clj
(define ls (list 1 2 3 4 5 6 7 8 9 10))

(take ls 5)
```

In the example above, the result of `(take ls 5)` is `(1 2 3 4 5)`.

---

### drop

`drop` creates a list from another list by dropping the `n` first elements of that list.
The form of a `drop` expression is `(drop list-exp n-exp)`.

Example that drops 5 elements from a list:
```clj
(define ls (list 1 2 3 4 5 6 7 8 9 10))

(drop ls 5)
```

Here `(drop ls 5)` evaluates to the list `(6 7 8 9 10)`.

---

### Merge


`merge` merges two lists that are ordered according to a comparator into
a single ordered list. The form of a `merge` expression is `(merge comparator-exp list-exp1 list-exp2)`.

Example:
```clj
(define a '(2 4 6 8 10 12))
(define b '(1 3 5))

(merge < a b)
```

Here `(merge < a b)` evaluates to the list `(1 2 3 4 5 6 8 10 12)`.

---

### Sort

`sort` orders a list of values according to a comparator. The sorting
algorithm used is an in-place merge-sort. A copy of the input list is created
at the beginning of the sort to provide a functional interface from the user's
point of view. The form of a sort expression is `(sort comparator-exp list-exp)`

Example:
```clj
(define ls '( 1 9 2 5 1 8 3))

(sort < ls)

```

Here `(sort < ls)` evaluates to the list `(1 1 2 3 5 8 9)`.

---


## Associations lists (alists)

Association lists (alists) are, just like regular lists, built out
of cons-cells. The difference is that an alist is a list of pairs
where the first element in each par can be thought of as a key and
the second element can be thought of as the value. So alists implement
a key-value lookup structure.

`(list '(1 . horse) '(2 . donkey) '(3 . shark))` is an example
of an alist with integer keys and symbol values.

### acons

The `acons` form is similar to `cons`, it attaches one more element
onto an alist. The element that is added consists of a key and a value
so `acons` takes one more argument than `cons`. The form of an
`acons` expression is `(acons key-expr val-expr alist-expr)`.
The `alist-expr` should evaluate to an alist but there are no checks
to ensure this.

Example that adds the key `4` and associated value `lemur` to
an existing alist.

```clj
# (acons 4 'lemur (list '(1 . horse) '(2 . donkey) '(3 . shark)))
> ((4 . lemur) (1 . horse) (2 . donkey) (3 . shark))
```

---

### assoc

The `assoc` function looks up the first value in an alist matching a given a key. 
The form of an `assoc` expression is `(assoc alist-expr key-expr)`

Example that looks up the value of key `2` in an alist.
```clj
# (assoc (list '(1 . horse) '(2 . donkey) '(3 . shark)) 2)
> donkey
```

---


### cossa

The `cossa` function looks up the first key in an alist that matches a given value. 
The form of an `cossa` expression is `(cossa alist-expr value-expr)`

Example that looks up the key for the value `donkey` in an alist.
```clj
# (cossa (list '(1 . horse) '(2 . donkey) '(3 . shark)) 'donkey)
> 2
```

---

### setassoc

The `setassoc` function destructively updates a key-value mapping in an
alist. The form of a `setassoc` expression is `(setassoc alist-expr key-expr value-expr)`. 


## Arrays (byte-buffers)

### bufcreate

Create an array of bytes. The
form of a `bufcreate` expression is `(bufcreate size-expr)`

Example that creates a 10 element buffer called data:

```clj
(define data (bufcreate 10))
```

Use the buffer extensions to operate `data`.
See the [VESC](https://github.com/vedderb/bldc/tree/master/lispBM#byte-arrays)
lbm documentation for details on operations on buffers.

---

### buflen

Returns the size of a buffer in number of bytes. The form
of an `buflen` expression is `(buflen buf-expr)` where
buf-expr has to evaluate into a buffer.

---

### bufget-[X]

Read a value from a buffer. The contents of a buffer can be read
as a sized integer or unsigned value using as many bytes from the buffer
as the X portion of the function name implies. 
The form of a bufget expression is `(bufget-[X] buf-expr ix-expr)` where
`ix-expr` evaluates to a number indicating the byte position to start
reading from.

- `bufget-i8`
- `bufget-i16`
- `bufget-i24`
- `bufget-i32`
- `bufget-u8`
- `bufget-u16`
- `bufget-u24`
- `bufget-u32`

Example that reads an u8 from a buffer at position 1:

```clj
(define buf [1 2 3 4])

(bufget-u8 buf 1)
```

---

### bufset-[X]

The `bufset` functions performs a destructive updates to a buffer.
The form of a `bufset` expression is `(bufset-[X] buf-expr ix-expr val-expr)`
where `ix-expr` evaluates to a number indicating where in the buffer to
start writing and `val-expr` is the value to write.


- `bufset-i8`
- `bufset-i16`
- `bufset-i24`
- `bufset-i32`
- `bufset-u8`
- `bufset-u16`
- `bufset-u24`
- `bufset-u32`

Example that updates position 1 in a buffer:

```clj

(define buf [1 2 3 4])

(bufset-u8 buf 1 100)
```

---

### bufclear

To clear a byte array the function bufclear can be used:

```clj
(bufclear arr optByte optStart optLen)
```

Where arr is the byte array to clear, optByte is the optional argument
of what to clear with (default 0), optStart is the optional argument
of which position to start clearing (default 0) and optLen is the
optional argument of how many bytes to clear after start (default the
entire array). Example:

```clj
(bufclear arr) ; Clear all of arr
(bufclear arr 0xFF) ; Fill arr with 0xFF
(bufclear arr 0 5) ; Clear from index 5 to the end
(bufclear arr 0 5 10) ; Clear 10 bytes starting from index 5
(bufclear arr 0xAA 5 10) ; Set 10 bytes to 0xAA starting from index 5
```
---

### Byte-array literal syntax

Byte-array (buffer) literals can be created using the `[` and `]` syntax to enclose
values to initialize the array with. The `[` and `]` syntax is complete
resolved in the parser and thus cannot contain arbitrary lisp terms.
the values listed between the `[` and the `]` must be literals!

The form of the `[` and `]` syntax is `[ val1 ... valN ]`.

Example that creates a byte array
```clj
[ 1 2 3 4 5 6 7 8 9 10 ]
```

---

## Pattern-matching

### match

Pattern-matching is expressed using match. The form of a match expression is
`(match expr (pat1 expr1) ... (patN exprN))`. Pattern-matching compares
the shape of an expression to each of the `pat1` ... `patN`
and evaluates the expression `exprM` of the pattern that matches.
In a pattern you can use a number of match-binders or wildcards: `_`, `?`, `?i`,`?u`,`?float`.

For example the match expression below evaluates to 2.
```clj
(match 'orange
       (green 1)
       (orange 2)
       (blue 3))
```

---

### no_match

The `no_match` symbol is returned from pattern matching if
no case matches the expression.

    - Add a catch-all case to your pattern-matching. `_`.

---

### _

The underscore pattern matches anything.


An example that evaluates to `i-dont-know`
```clj
(match 'fish
       (horse 'its-a-horse)
       (pig 'its-a-pig)
       (_ 'i-dont-know))
```

---

### ?

The `?` pattern matches anything and binds that anything to variable.
Using the `?` pattern is done as `(? var)` and the part of the expression
that matches is bound to `var`.

An example that evaluates to 19.
```clj
(match '(orange 17)
       ((green (? n)) (+ n 1))
       ((orange (? n)) (+ n 2))
       ((blue (? n)) (+ n 3)))
```

---

### Match with guards

Patterns used in a match expressions can be augmented with a boolean
guard to further discern between cases. A pattern with a guard is of the
form `(pattern-expr guard-expr expr)`. A pattern with a guard, matches only
if the pattern structurally matches and if the guard-expr evaluates to true
in the match environment.

Example:
```clj
(match (x)
       ( (? y) (< y 0) 'less-than-zero)
       ( (? y) (> y 0) 'greater-than-zero)
       ( (? y) (= y 0) 'equal-to-zero))
```

---

## Concurrency

The concurrency support in LispBM is provided by the set of functions,
`spawn`, `wait`, `yeild` and `atomic` described below.  Concurrency in
LispBM is scheduled by a round-robin scheduler that splits the runtime
system evaluator fairly (with caveats, below) between all running processes.

When a process is scheduled to run, made active, it is given a quota of
evaluator "steps" to use up. The process then runs until that quota is
exhausted or the process itself has signaled it wants to sleep by
yielding or blocking (for example by waiting for a message using the
message passing system).

A process can also request to not be "pre-empted" while executing a
certain expression by invoking `atomic`. One should take care to make
blocks of atomic code as small as possible as it disrupts the fairness
of the scheduler. While executing inside of an atomic block the process
has sole ownership of the shared global environment and can perform
atomic read-modify-write sequences to global data.


### spawn

Use `spawn` to launch a concurrent process. Spawn takes a closure and
arguments to pass to that closure as its arguments. The form of a
spawn expression is `(spawn opt-name opt-stack-size closure arg1
... argN)`.

Each process has a runtime-stack which is used for the evaluation of
expressions within that process. The stack size needed by a process
depends on
 1. How deeply nested expressions evaluated by the process are.
 2. Number of recursive calls (Only if a function is NOT tail-recursive).
 3. The Number of arguments that functions called by the process take.

Having a stack that is too small will result in a `out_of_stack` error.

The default stack size is 256 words (1K Bytes) and should be more than
enough for reasonable programs. Many processes will work perfectly
fine with a lot less stack. You can find a good size by trial and error.

---

### spawn-trap

Use `spawn-trap` to spawn a child process and enable trapping of exit
conditions for that child. The form of a `spawn-trap` expression is
`(spawn-trap opt-name opt-stack-size closure arg1 .. argN)`.  If the
child process is terminated because of an error, a message is sent to
the parent process of the form `(exit-error tid err-val)`. If the
child process terminates successfully a message of the form `(exit-ok
tid value)` is sent to the parent.

Example:
```clj
(spawn-trap my-thread)

(recv  ((exit-error (? tid) (? e)) ...)
       ((exit-ok    (? tid) (? v)) ...))
```

---

### self

Use `self` to obtain the thread-id of the thread in which `self` is evaluated.
The form of a `self` expression is `(self)`. The thread id is of an integer type.

Example:
```clj
# (self)
> 314
```
---

### wait

Use `wait` to wait for a spawned process to finish.
The argument to `wait` should be a process id.
The `wait` blocks until the process with the given process id finishes.
When the process with with the given id finishes, the wait function returns True.


Be careful to only wait for processes that actually exist and do
finish. Otherwise you will wait forever.

---

### yield

To put a process to sleep, call `yield`. The argument to `yield`
is number indicating at least how many microseconds the process should sleep.

---

### atomic

`atomic` can be used to execute a LispBM one or more expression without allowing
the runtime system to switch process during that time.

An example that atomically perfoms operations a,b and c.

```clj
(atomic
     a
     b
     c)
```
---

### exit-ok

The `exit-ok` function terminates the thread in a "successful" way and
returnes a result specified by the programmer. The form of an
`exit-ok` expression is `(exit-ok value)`.  If the process that calls
`exit-ok` was created using `spawn-trap` a message of the form
`(exit-ok tid value)` is be sent to the parent of this process.

---

### exit-error

The `exit-error` function terminates the thread with an error
specified by the programmer.  The form of an `exit-error` expression
is `(exit-error err_val)`. If the process that calls `exit-error` was
created using `spawn-trap` a message of the form `(exit-error tid
err_val)` is sent to the parent of this process.

---

## Message-passing

### send

Messages can be sent to a process by using `send`. The form
of a `send` expression is `(send pid msg)`. The message, msg,
can be any LispBM value.

---

### recv

To receive a message use the `recv` command. A process
will block on a `recv` until there is a matching message in
the mailbox.
The `recv` syntax is very similar to [match](./lbmref.md#match).

Example where a process waits for an integer `?i`.
```clj
(recv ( (?i n) (+ n 1) ))
```

---

### recv-to

Like [recv](./lbmref.md#recv), `recv-to` is used to receive
messages but `recv-to` takes an extra timeout argument.

The form of an `recv-to` expression is
```clj
(recv-to timeout-secs
                (pattern1 exp1)
                ...
                (patternN expN))
```

If no message is received before the timout, the message `timeout` is
delivered to the waiting process. This `timeout` message can be handled
in one of the receive patterns.

Example
```clj
(recv-to 0.5
         ( timeout (handle-timeout))
         ( _       (do-something-else)))
```

---

### set-mailbox-size

Change the size of the mailbox in the current process.
Standard mailbox size is 10 elements.

Example that changes mailbox size to 100 elements.
```clj
(set-mailbox-size 100)
```

---

## Flat values

Lisp values can be "flattened" into an array representation. The flat
representation of a value contains all information needed so that the
value can be recreated, "unflattened", in another instance of the
runtime system (for example running on another microcontroller).

Not all values can be flattened, custom types for example cannot.

### flatten

The `flatten` function takes a value as single argument and returns the flat representation if
successful. A flatten expression has the form `(flatten expr)`. Note that `expr` is evaluated
before the flattening.

Example:
```clj
# (define a (flatten (+ 1 2)))
> ""
```
The example above creates a byte-array representating the the value `3`.

```clj
# (define a (flatten '(+ 1 2)))
> "+"
```

Now the byte-array contains a representation of the list `(+ 1 2)`

---

### unflatten

`unflatten` converts a flat value back into a lisp value. Te form of an
`unflatten` expression is `(unflatten flat-value)`

Example:
```clj
# (define a (flatten (+ 1 2)))
> ""
# (unflatten a)
> 3
```

and:

```clj
# (define a (flatten '(+ 1 2)))
> "+"
# (unflatten a)
> (+ 1 2)
```

## Macros

lispBM macros are created using the `macro` keyword. A macro
is quite similar to [lambda](./lbmref.mb#lambda) in lispBM except that
arguments are passed in unevaluated. Together with the code-splicing
capabilities given by [quasiquotation](./lbmref.md#quasiquotation), this
provides a powerful code-generation tool.

A macro application is run through the interpreter two times. Once to
evaluate the body of the macro on the unevaluated arguments. The result of
this first application should be a program. The resulting program then goes
through the interpreter again to compute final values.

Given this repeated evaluation, macros are not a performance boost in
lispbm.  Macros are really a feature that should be used to invent new
programming abstractions in cases where it is ok to pay a little for
the overhead for benefits in expressivity.

### macro

The form of a `macro` expression is: `(macro args body)`

Some lisps provide a `defun` operation for defining functions
with a bit less typing. The example below defines a `defun` macro.
```clj
(define defun (macro (name args body)
                     `(define ,name (lambda ,args ,body))))
```
With this macro the function `inc` that adds 1 to its argument
can be defined as:
```clj
(defun inc (x) (+ x 1))
```

---

## Call With Current Continuation

"Call with current continuation" is called `call-cc` in LBM.
Call with current continuation saves the "current continuation", which encodes what
the evaluator will do next, into an object in the language. This encoded
continuation object behaves as a function taking one argument.


The `call-cc` should be given a function, `f`, as the single argument. This
function, `f`, should also take a single argument, the continuation.
At any point in the body of `f` the continuation can be applied to
a value, in essense replacing the entire `call-cc` with that value. All side-effecting operations
operations up until the application of the continuation will take effect.

From within a `call-cc` application it is possible to bind the continuation to a global variable
which will allow some pretty arbitrary control flow.


The example below creates a macro for a `progn` facility that
allows returning at an arbitrary point.
```clj
(define do (macro (body)
                  `(call-cc (lambda (return) (progn ,@body)))))
```
The example using `do` below makes use of `print` which is not a
built-in feature of lispBM. There are just to many different ways a programmer may
want to implement `print` on an microcontroller. Use the lispBM extensions
framework to implement your own version of `print`
```clj
(do ((print 10)
     (return 't)
     (print 20)))
```
In the example above only "10" will be printed.
Below is an example that conditionally returns.
```clj
(define f (lambda (x)
            (do ((print "hello world" \#newline)
                 (if (= x 1)
                     (return 't)
                     nil)
                 (print "Gizmo!" \#newline)))))
```

---

## Error handling

If an error occurs while evaluating a program, the process that runs
that program is killed.  The result of the killed process is set to an
error symbol indicating what went wrong.

If the process was created using `spawn` (or equivalently, started by a
issuing a command in the repl), the process dies and an error message
is presented over the registered printing callback (dependent on how LispBM
is integrated into your system). The `ctx_done_callback` is also called
and performs other integration dependent tasks related to the shutting down
of a process.

If the process was created using `spawn-trap`, in addition to the
above, a message is sent to the parent process (the process that
executed the spawn-trap) containing information about the process that
struck an error. See <a href="#spawn-trap">spawn-trap</a>.
The parent process can now choose to restart the process that crashed
or to take some other action.

---

## Error Symbols

### read_error

The `read_error` symbol is returned if the reader cannot
parse the input code.

Read errors are most likely caused by syntactically incorrect input programs.

    - Check that all opening parenthesis are properly closed.

---

### type_error

The `type_error` symbol is returned by built-in functions or extensions
if the values passed in are of incompatible types.

---

### eval_error

The `eval_error` symbol is returned if evaluation could
not proceed to evaluate the expression. This could be because the
expression is malformed.

Evaluation error happens on programs that may be syntactically correct
(LispBM has a very low bar for what is considered syntactically correct),
but semantically nonsensical.

    - Check the program for mistakes.
    - Are your parenthesis enclosing the correct subterms?
    - Check that you haven't written, for example, (1 + 2) where it should be (+ 1 2).

---

### out_of_memory

The `out_of_memory` symbol is returned if the heap is full and running
the garbage collector was not able to free any memory up.

The program you have written requires more memory.

    - Increase the heap size.
    - Rewrite the application to use less memory.

---

### fatal_error

The `fatal_error` symbol is returned in cases where the
LispBM runtime system cannot proceed. Something is corrupt and it is
not safe to continue.

    - If this happens please send the program and the full error message
      to blog.joel.svensson@gmail.com. It will be much appreciated.

---

### out_of_stack

The `out_of_stack` symbol is returned if the evaluator
runs out of continuation stack (this is its runtime-stack). You are
most likely writing a non-tail-recursive function that is exhausting all
the resources.

    - Check your program for recursive functions that are not tail-recursive
      Rewrite these in tail-recursive form.
    - If you spawned this process in a small stack. For example (spawn 10 prg),
      try to spawn it with a larger stack.

---

### division_by_zero

The `division_by_zero` symbol is returned when dividing by zero.

    - Check your math.
    - Add 0-checks into your code at a strategic position.

---

### variable_not_bound

The `variable_not_bound` symbol is returned when evaluating a
variable (symbol) that is neighter bound nor special (built-in function).


## Flash memory

Flash memory can be used to store data and functions that are constant.
Things can be moved to flash explicitly using the `move-to-flash` function
or as part of the reading procedure. To move things automatically to flash during
reading, there are `@`directives.

---

### @const-symbol-strings

if `@const-symbol-strings` directive is placed in a file, symbols will be created
in flash memory instead of the arrays memory.

---

### @const-start

`@const-start` opens a block of code where each global definition is
moved to constant memory (flash) automatically. This can be used only together with the
incremental reader (such as `read-eval-program`).

A `@const-start` opened block should be closed with a `@const-end`. Constant blocks
cannot be nested.

Example:

```clj
@const-start

(defun f (x) (+ x 1))  ; a function stored in constant memory

@const-end

(+ f 2)
```

---

### @const-end

`@const-end` closes an block opened by `@const-start`.

---

### move-to-flash

A value can be moved to flash storage to save space on the normal evaluation heap or lbm memory.
A `move-to-flash` expression is of the form `(move-to-flash sym opt-sym1 ... opt-symN)`.
The symbols `sym`, `opt-sym1 ... opt-symN` should be globally bound to the values you want moved
to flash. After the value has been moved, the environment binding is updated to point into flash
memory. **CAUTION** This function should be used carefully. Ideally a value should be moved
to flash immediately after it is created so there is no chance that other references to original value
exists.

Example that moves an array to flash storage:

```clj
(define a [1 2 3 4 5 6])

(move-to-flash a)
```

Example that moves a list to flash storage:

```clj
(define ls '(1 2 3 4 5))

(move-to-flash ls)
```

Functions can be moved to flash storage as well:

```clj
(defun f (x) (+ x 1))

(move-to-flash f)
```

---

## Types

### type-list

---

### type-i

A value with type `type-i` occupy 28bits on the 32 bit version of LBM and
56bits on the 64bit version.

---

### type-u

A value with type `type-u` occupy 28bits on the 32 bit version of LBM and
56bits on the 64bit version.


---

### type-float

---

### type-i32

---

### type-u32

---

### type-i64

---

### type-u64

---

### type-double

---

### type-array

---

### type-symbol

---

### type-char

---

### type-ref

---

### type-channel

---

## Type convertion functions

### to-byte

Convert any numerical value to a byte.
If the input is not a number the output of this function will be 0.

---

### to-i

Convert a value of any numerical type to an integer.
The resulting integer is a 28bit value on 32bit platforms and 56 bits on 64 bit platforms.
If the input is not a number the output of this function will be 0.

---

### to-u

Convert a value of any numerical type to an unsigned integer.
The resulting integer is a 28bit value on 32bit platforms and 56 bits on 64 bit platforms.
If the input is not a number the output of this function will be 0.

---

### to-i32

Convert any numerical value to a 32bit int.
If the input is not a number the output of this function will be 0.

---

### to-u32

Convert any numerical value to a 32bit unsigned int.

---

### to-float

Convert any numerical value to a single precision floating point value.
If the input is not a number the output of this function will be 0.

---

### to-i64

Convert any numerical value to a 64bit int.
If the input is not a number the output of this function will be 0.

---

### to-u64

Convert any numerical value to a 64bit unsigned int.
If the input is not a number the output of this function will be 0.

---

### to-double

Convert any numerical value to a double precision floating point value.
If the input is not a number the output of this function will be 0.

---





