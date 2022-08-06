
# Chapter 1: Introduction to programming in LispBM

LispBM (from now on called LBM) is a Lisp dialect that was implemented
to be run on small resource constrained systems. The look-and-feel of
LBM has been very much influenced by the series of videos based on
the SICP book (Structure and Interpretation of Computer Programs) by
Harold Abelson, Gerald Jay Sussman and Julie Sussman. The awesome
series of videos about Lisp programming can be found
[here](https://www.youtube.com/playlist?list=PL8FE88AA54363BC46). Note
that LBM is not 100% compatible with the code you see in the video series
but this is quite OK, there are many slightly different flavors of Lisps.

LBM itself implements the concurrency, communication and a basic set
of Lisp functionality such as arithmetic. The idea with LBM is that it
should be embedded into some other embedded system, or other,
application and functionality specific to that application is exposed
to LBM via so-called extensions. As a result of that it is for example
not possible to print something from LBM. Printing is a simple example
of something that can be implemented in many different ways on many
different platforms (uart, bluetooth USB-CDC). It is up to the person
integrating LBM into a system to provide these interfaces.

That said, there is an example REPL (Read Evaluate Print Loop) that
can be run on X86 Linux and that is what will be used in this
introductory chapter to get started.

## Building the example REPL

Clone the LispBM repository from [GitHub](https://github.com/svenssonjoel/lispBM).

```
git clone https://github.com/svenssonjoel/lispBM.git
```
Then go into the `lispBM` directory and the `repl` subdirectory.

```
cd lispBM
cd repl
```

Now you have the choice of compiling the REPL either a 32bit application
or a 64bit one. To compile as a 32bit application just type `make`, this
requires that you have 32bit libraries installed on you Linux system. The
other alternative is to compile as 64bit using `make all64`.

```
make
```
or

```
make all64
```

If all goes well there should now be an executable called `repl` in
the current directory. To start the repl type:

```
./repl
```

which should present the following prompt:


```
Array extensions loaded
String extensions loaded
Math extensions loaded
Extension added.
Extension added.
Lisp REPL started!
Type :quit to exit.
     :info for statistics.
     :load [filename] to load lisp source.
#
```

If you try out the `:info` command you are presented with some
information about the current state of the runtime system.

```
# :info
--(LISP HEAP)-----------------------------------------------
Heap size: 16384 Bytes
Used cons cells: 0
Free cons cells: 2048
GC counter: 0
Recovered: 0
Recovered arrays: 0
Marked: 0
--(Symbol and Array memory)---------------------------------
Memory size: 2048 Words
Memory free: 1916 Words
Allocated arrays: 0
Symbol table size: 676 Bytes
```

There is a set of LBM functions mostly for basic operations on lists
that the REPL will load dynamically when first used. One of these
functions is called `iota` and it creates a list enumerating numbers
from 0 up to the argument provided.

To test if this dynamic loading of the library function works type
`(iota 1024)` and press enter.

```
# (iota 1024)
> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 ...
```

If things seem to work so far, lets go on and play some with the repl.

---
**NOTE**

You can also install the REPL into the `~/.local/bin` directory
by issuing the command `make install`. This installs the REPL binary
as the command `lbm` and it can be started from anywhere if you
have your `.local/bin` directory added to your path.

---

## Playing with the REPL

The REPL allows you to enter expressions and have them evaluated.
For example we can type `(+ 1 2)` at the prompt and get a response.

```
# (+ 1 2)
> 3
```

The REPL informs us that it is loading `(+ 1 2)`, and that it started
a context with ID 144 to evaluate the expression in.  When then
context then finishes execution the REPL presents `3` which means that
the result of `(+ 1 2)` was computed to be `3`. 

The example REPL provides an extension for printing things, for
example strings:

```
# (print "hello world!")
hello world!
> t
```

The program above implements "hello world" as you can see on the
output presented above. The `t` is the return value of print
symbolising "true" for success. The print function baked into the REPL
is capable of printing a lot of different LBM values, not just
strings.  for example:

```
# (print 10)
10
> t
```

You can print many things at once by providing more arguments to print.

```
# (print "The year " 1492)
The year 1492
> t
```

We will see what else print can print as we progress through the manual.

Now let's see if we can implement a small game right in the REPL! Inspired
by the book "Land of Lisp" we can try a somewhat simplified version
of the "guess-my-number" game that can be typed directly into the REPL
in small number of steps.

The objective of the game is to have the computer guess what number you,
the user, is thinking about.

First we specify in what range of numbers in which we are allowed to pick
a random number.

Enter the following into the REPL and hit enter:
```
(define small 1)
```
and the REPL replies:

```
# (define small 1)
> small
```
Our number must be larger than or equal to 1.

```
(define big 100)
```
Our number must be smaller than or equal to 100.

`define` associates a variable with a value. Here we have defined `small` to
be 1 and `big` to be 100. We can ask the REPL about these values now if we want
by typing `small` or `big` into the REPL and pressing enter.


```
# small
> 1
```

To get a guess from the computer we call a function called
`guess-my-number` that is implemented as follows:

```
(define guess-my-number (lambda () (/ (+ small big) 2)))
```
This time define is used to associate a name with a function. The
function itself is created using `lambda`. This function takes no
arguments, this is what the `()` means following `lambda`. After the
empty list of arguments comes the body of function that computes (small + big) / 2.
which means the computer will guess in the middle of the range.

Now we can ask the computer to take a guess by typing
`(guess-my-number)` at the REPL prompt. The parenthesis around
`guess-my-number` means function application.

```
# (guess-my-number)
> 50
```
Now, if the computer's guess is wrong we can help it by saying `(bigger)` or `(smaller)`. 
Let's implement these functions.

```
(define smaller (lambda () (define big (- (guess-my-number) 1))))
```
The `smaller` function takes no arguments and works but setting the upper end
of the range for the computer to guess in to the current guess - 1. The current
guess is obtained by calling the `guess-my-number` function. `define` is used
in the function body here to re-define the value of `big`.

The `bigger` function works similarly but moves the lower end of the range
up to the current guess.

```
(define bigger (lambda () (define small (+ (guess-my-number) 1))))
```

Now we have all the functions we need to play the game.

Let's say that we think about the number 7 and ask the computer to guess:

```
# (guess-my-number)
> 50
```

50, thats to high, type `(smaller)` into the REPL and press enter.
The we ask the computer to guess again

```
# (guess-my-number)
> 25
```
25, that is still too high, `(smaller)`, enter.
Guess again computer!

```
# (guess-my-number)
> 12
```

12, too high again, `(smaller)`, enter.
Guess again!

```
# (guess-my-number)
> 6
```

6, is too small! `(bigger)`, enter.
Guess please ;)

```
# (guess-my-number)
> 9
```

No, no, not 9. `(smaller)`, enter.
`(guess-my-number)`

```
# (guess-my-number)
> 7
```
Yay! Go computer!


## LBM Syntax and Semantics

Languages in the Lisp family use the same data structure to represent
programs as they do to organize data. This data structure is the *list*.
As a result the syntax relating to lists is very important get down early.
Languages that use the same representation for data and code, are said to
have a property called homoiconicity. The homoiconicity property is valuable
in situations where you are doing "meta-programming", writing programs
that result in programs, as for example when writing macros. In the very
beginning it can, however, be a bit confusing! But hang in there and
the benefits will become clear over time.


### Lists

Lists in LBM are enclosed in parentheses and can be arbitrarily nested.
So `(1 2 3)` is a list and `(1 (2 3) 4)` is a list where the second element
is again a list. Now, if we try to write `(1 2 3)` into the REPL and hit enter,
the REPL wont be happy with us!

```
# (1 2 3)
***	Error: eval_error

> eval_error
```

Typing `(1 2 3)` into the REPL resulted in an eval error. This is because
the default way in which lists are understood by LBM is as code. And
`(1 2 3)` is not a valid LBM program.

When a list such as `(a b c)` (for any a,b,c) is entered into the REPL
the LBM evaluator will assume that this is an application of the function
`a` to the arguments `b` and `c`. The list `(+ 1 2)` on the other hand
is a valid program as the first element of the list is the addition function
and it can be applied to the arguments `1` and `2`.

```
# (+ 1 2)
> 3
```

So, if lists that we give to the evaluator are assumed to be code, how
do we create a list of data? We will see many answers to this
questions throughout this manual but to begin with we will use a
function application to create a list of data. LBM provides a function
called `list` that takes an arbitrary number of arguments and returns
a list containing those values. Now the list `(list 1 2 3)` is
something that makes sense to the LBM evaluator, because the first
element is the function `list`.

```
# (list 1 2 3)
> (1 2 3)
```

In a function application the arguments are evaluated before the function
at the head of the list is applied to them.

LBM lists are constructed from so-called cons-cells. Each cons-cell
has two fields and in each field, any LBM value can be stored. The
list we constructed earlier using `(list 1 2 3)` takes up three cons
cells.  The first of these cons-cells has the value `1` in the first
(called `car` for historical reasons) fields of the cons-cell, the
second field (called `cdr`) holds a pointer to the "rest of the list".
In other words, lists are linked-lists. The very last and unused `cdr` field
holds the symbol `nil` terminating the linked-list. `nil` is considered
an "empty-list".

### Symbols

Symbols are very fundamental building blocks of LBM programs
and data. A symbol is made up from a number of characters and we have
seen some examples already, for example `list` or `guess-my-number`
from the dive-in-intro. Symbols are used to name data or functions
but can also be used as values in themselves. For this introduction
to symbols we will focus on their usefulness for giving names to things.

To associate a value with a symbol, we use `define`. In this use-case
of symbols, you can think of them as basically variables. To define a
variable give, for example, the list `(define a 10)` to the REPL.

```
# (define a 10)
> a
```

This sets up an association between the symbol `a` and the value 10. And
now if we enter `a` into the REPL and press enter, the REPL will reply
with `10`.

```
# a
> 10
```

LBM is case-insensitive when it comes to symbols, so `a` and `A` is the
considered to be the same symbol.

Characters that are ok as the first character in a symbol:
```
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/=<>#
```

Characters that are ok on any other position within a symbol name string:
```
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/=<>!?
```

---
**NOTE**

Symbols starting with the character `#` are allocated by the LBM
system in a special way in order to make value lookup (if a value
is bound to that symbol) more efficient. So, symbols should not start with
`#` unless you specifically want to use that symbol as a binding of a value
that you are going to use a lot and where efficiency will matter a lot.

---
**NOTE**

Symbols starting with `ext-` are allocated in the table of extensions.
This means that you should not create symbols starting with `ext-`
unless you are going to associate that symbol with an
extension. Currently the only way to associate an `ext-` symbol with
an extension is by using the dynamic native code loader in the [VESC
flavor](https://github.com/vedderb/bldc/tree/master/lispBM) of LBM.
In the VESC LBM, loading of these dynamic extensions is done via an
extension called `load-native-lib`.

---



### An important concept with an unremarkable name: Quote

The `quote` operation is very important as it will turn up a lot as
we move ahead. Quote is written as `'` in code and it tells the evaluator
to **not** evaluate the following expression. As we saw earlier, anything we
enter into the REPL will be evaluated, and because of that we could not type
in `(1 2 3)` without getting an error. Typing in `'(1 2 3)` is perfectly
fine and results in the list `(1 2 3)`.

```
# '(1 2 3)
> (1 2 3)
```

Any LBM expression can be quoted and essentially then viewed as a
data-structure rather than as code. For example you can quote any
symbol:

```
# 'i-am-a-symbol
> i-am-a-symbol
```

Quoted symbols is very common to see when writing programs that use
symbols not as variables but as data in them self. A symbol can for
example be used as a tag or label. For example if you want to keep
track of your apples and bananas, you could create a data-structure
like this `(list (list 'apples 10) (list 'bananas 15))`.

```
# (list (list 'apples 10) (list 'bananas 15))
> ((apples 10) (bananas 15))
```

Another example is that you can quote an addition expression such as
`(+ 1 2)`:

```
# '(+ 1 2)
> (+ 1 2)
```

the result of this quoted addition is a list with the elements `+`,`1`
and `2`.

Quote is a bit interesting as it does not follow the pattern of a
function application, which is a list where the first element is
assumed to be function.  The quote mark, `'` is placed before the
thing it "operates" on and it is not not enclosed in parenthesis! Now,
this is just syntax and as the LBM code is read by the parser all
things of the form `'a` is expanded into `(quote a)` and `(quote a)`
fits the familiar pattern very well. Still, `quote` is not quite a
function and this is because when quoting something we want the thing
we quote to be returned to us unevaluated! In a function application
the arguments are evaluated.  So `quote` falls into a small set of
operations called **special forms**. All special forms are listed
in a section at the end of this chapter.

### Quasiquote

There is a close relative of quote `'` called quasiquote `` ` `` that
allows you to mix between not evaluating within an expression. The
quasiquote back-tick is then used together with the `,` operation, back-tick
to "not-evaluate" and comma to "do-evaluate-this-part".

For example in `` `(+ 1 ,(+ 2 3))``, the subexpression `(+ 2 3)` will be
evaluated because it has the comma sign prepended. The result of this expression
is:

```
#  `(+ 1 ,(+ 2 3))
> (+ 1 5)
```

To insert the result of evaluating an expression into a data-structure
using `,` is called to "splice". There is another splicing operator
that can be used together with the quasiquote and it is the `,@`.
This `,@` operation should be followed by some expression that
evaluates into a list, then all the element of that resulting list
is spliced into the data-structure. For example `` `(+ 1 ,@(list 2 3 4))``
which results in the list `(+ 1 2 3 4)`.

```
# `(+ 1 ,@(list 2 3 4))
> (+ 1 2 3 4)
```

Usages of `` ` ``, `,` and `,@` are also expanded by the parser
when the program is read into applications of `quote` and different
list appending functions. So just as with `'` the back-tick and comma-at
is just surface syntax.

### Functions

In LBM you create functions using the special-form (keyword) `lambda`.
The `lambda` creates an unnamed function, that can be bound to a
variable (symbol) using `define` if you wish.

A function has the form `(lambda parameter-list function-body)` where
the `parameter-list` could be for a two argument function `(x y)`, any
list of "non-special" symbols is fine. The function body is an
expression which can refer to the symbols in the parameter list. A
concrete example of a function is `(lambda (x y) (+ (* 2 x) y))` which
takes two arguments `x` and `y` and computes `2x + y`.

LBM supports higher-order-functions which means that functions can be
used as input to other functions, or be returned from a function as a
result. To enable this, an expression of the `(lambda parameter-list
function-body` form is evaluated into a function object called a
closure. This closure contains an environment containing bindings for
all the variables in the `function-body` that is not in the parameter
list. More about this over time in later chapters!

Application of function is, as we know, done by writing down a list
where the first element is function and the rest of the list are
arguments.  In the case of the `2x + y` anonymous function an
application would look like.

```
((lambda (x y) (+ (* 2 x) y)) 2 1)
```

The above expression is a list, the first element in this list is
`(lambda (x y) (+ (* 2 x) y))` and the rest of the list is `2` `1`. So
the evaluator will treat this as an application and apply the function
at the heap of the list to the arguments.


```
# ((lambda (x y) (+ (* 2 x) y)) 2 1)
> 5
```

---
**NOTE**

Actually, the function application form of an expression `(f a b c)`
the `f` is also evaluated and should result in an applicable function form.
lambdas evaluate into closures which are the function objects that in the
end are applied to the arguments.

---

To give a function a name, use `define`. Let's give a name to the
function above:

```
(define twoxplusy (lambda (x y) (+ (* 2 x) y)))
```

Now it is possible to apply the function by placing the symbol `twoxplusy`
at the head of an application form, for example `(twoxplusy 2 1)`:

```
# (twoxplusy 2 1)
> 5
```

### Macros

Together with quote and quasiquote, macros give powerful
meta-programming capabilities. With macros you can define new language
constructs to use within your programs. Macros is where a lot of
the power of lisp-like languages comes from.

Creating macro is done in a way that is, on the surface, very similar to
creating a function. Here it is `(macro parameter-list macro-body)`
compared to `(lambda parameter-list function-body)` for a function.

The `macro-body`, in an LBM program, should be an expression that
evaluates into a program (this is where quasiqoutation becomes really
useful).

A big difference between a function application and a macro
application is that when you apply a macro to arguments, those
arguments are not evaluated. The arguments are available to the
`macro-body` in unevaluated form and you, the macro implementor,
decides what to do with those arguments (evaluate or not).

Let's look at a basic macro here and then spend some entire
later chapter on the topic.

``` lisp
(define defun (macro (name args body)
    `(define ,name (lambda ,args ,body))))
```

The above LBM code defines a macro and gives it the name `defun` (for
define function). The macro itself takes 3 arguments, `name`, `args`
and `body`. The `macro-body` is the expression `` `(define ,name
(lambda ,args ,body))``.

The purpose of the `defun` macro is to let you define a function with
a bit less typing on the keyboard. Instead of writing `(define f
(lambda (x) (+ 100 x)))` you would write `(defun f (x) (+ 100 x))`.

In the macro application `(defun f (x) (+ 100 x))` we see that `defun`
is applied to `f`, `(x)` and `(+ 100 x)`. Because `defun` is a macro,
the arguments are passed into the macro body unevaluated and will be
spliced into the expression `` `(define ,name (lambda ,args ,body))``,
which results in `(define f (lambda (x) (+ 100 x)))`. After splicing
in the unevaluated arguments into the macro-body, the resulting program is
evaluated and, in this case, the definition of `f` takes place.

---
**NOTE**

A macro body can execute arbitrary LBM code as long as the result
in the end is a program.

---

---
**NOTE**

The `defun` macro is quite useful, so we will be using it from now
on. This macro is available to you in the REPL.

---


### Conditionals and truth

in LBM, only the symbol `nil` is considered to be false, any
other value is considered true. If you want to explicitly express
"true" there is a symbol specifically for this purpose, `t`.

So `(if t 1 2)` evaluates to `1`. `t` is a special symbol that cannot
be redefined (using `define` to any new value), by default the symbol
`t` evaluates to `t`.

The syntax for a conditional is `(if condition-expr then-expr else-expr)`
and if `condition-expr` evaluates to something that is considered true,
the `then-expr` is evaluated. If `condition-expr` evaluates to nil,
the `else-expr` is evaluated.

LBM has two different equality checking functions, one is called `eq` and
it can be used to compare any LBM value to any other and utilizes what is
called structural equality. The other equality operation is `=` and
it is specifically for comparing numerical values to each other. For example
`(= 'monkey 'ostrich)` will result in a `type-error` but the following
are all valid expressions: `(eq 'monkey 'ostrich)`, `(eq 1 1)` and
`(= 1 2)`.

let's define a function that recognizes monkeys.

``` lisp
(defun monkey? (animal) (eq animal 'monkey))
```

The `monkey?` function returns `t` for monkeys

``` lisp
# (monkey? 'monkey)
> t
```
and it returns false (`nil`) for anything else.

``` lisp
# (monkey? 'ostrich)
> nil
```

---
**NOTE**

In this section we have used quoted symbols quite a bit. For example
we checked for equality between a monkey and an ostrich by doing `(eq
'monkey 'ostrich)`. quoting symbol arguments like this is quite normal
when we are interested in the symbol itself as the value, and not what
it would happen to be bound to.

---

You can combine truth values using the boolean operations `and`, `or`
and negate a condition using `not`. The `and` and `or` operations
takes an arbitrary number of arguments and are short-circuiting, that
is, they terminate as soon as the resulting value can be known. For an
`and` operation that means it can return false (`nil`) as soon as it
sees that one of its arguments are false. For `or` it can return true as
soon as it sees a non-nil value.

### Environments: global and local

We have already touched upon environments earlier when we used `define` to
globally associate a value with a symbol. You can define an association between
a value and a symbol anywhere and as soon as that definition has evaluated
the association is visible everywhere. You can also redefine a symbol to be
associated with another value.

Type `(define a 10)` into the REPL and press enter.

If you now enter `a` into the repl and press enter, the REPL will reply as:

```
# a
> 10
```

The REPL also supports a command `:env` that shows information about current
global bindings:

```
# :env
Environment:
  (a . 10)
Variables:
```

The output above shows that there is one binding present in the global
environment and that is the binding of `10` to the symbol `a`. The output
also shows that there are no "Variable"-bindings currently.

These "Variables" are a different kind of global variable supported by LBM.
A variable is created whenever you define a symbol starting with the character
`#`, and the value is stored in a way that is more efficient to look up.
There is a limited number of these variables (specifiable by the programmer
that integrates LBM into a system) so use them only where performance matter
the most.

For example type `(define #a 100)` into the REPL and press enter. Then
execute the `:env` command again. The output should now read:

```
# :env
Environment:
  (a . 10)
Variables:
  #a = 100
```

Local environments are created using the `let` special form in LBM and
a let expression takes the following form:

```
(let ((binder1 value1-exp)
      (binder2 value2-exp)
      ...
      (binderN valueN-exp))
  body-expr)
```
The binders `binder1` to `binderN` are all symbols and the bindings are
only visible inside of the `body-expr`. If a local binding shares the same
name as a global one, inside of the `body-expr`, the local binding shadows
the global one.

Variables, `#`-symbols cannot be bound locally using `let`. Evaluating a
`#`-symbol will always yield the globally defined value.

### Values and types

LBM is a dynamically typed language which means that type information
is carried along with the values at runtime of the program and that
"variables" or bindings are essentially untyped.

To check what type something is in LBM, you can apply the function
`type-of` to the value you are interested in.

```
# (type-of 365)
> type-i
```

In the example above we see that the value `365` has the type `type-i`
for integer.

Below is a list of LBM types:

| Type        | Description                                                                           | Example/syntax        |
| ---         | ---                                                                                   |  ---                  |
| type-char   | 8 bit quantity representing a character.                                              | `\#a`, `\#X`          |
| type-byte   | 8 bit quantity. type-byte is an alias for type-char.                                  | `1b`, `255b`          |
| type-i      | 28bit integer on 32bit platforms, 56bit integer on 64bit platforms.                   | `1`, `-5`             |
| type-u      | 28bit unsigned integer on 32bit platforms, 56bit unsigned integer on 64bit platforms. | `1u`, `5u`            |
| type-i32    | 32bit integer.                                                                        | `1i32`, `-5i32`       |
| type-u32    | 32bit unsigned integer.                                                               | `1u32`, `0xFF`        |
| type-i64    | 64bit integer.                                                                        | `1i64`, `-5i64`       |
| type-u64    | 64bit unsigned integer.                                                               | `1u64`, `0xFFu64`     |
| type-float  | Single precision floating point value.                                                | `3.14`, `-6.28`       |
| type-double | Double precision floating point value.                                                | `3.14f64`, `-6.28f64` |
| type-list   | LBM linked list.                                                                      | `(list 1 2 3)`        |
| type-symbol | Symbol                                                                                | `monkey`, `define`    |
| type-array  | LBM array as created using `array-create` or a string.                                | `"Hello world"`       |


Because LBM associates types with runtime values, this type
information has to be stored somewhere. In LBM the type information is
stored as part of the value word in in the case of `type-char`,
`type-i`, `type-u`, `type-list` and `type-symbol`. This is the reason
why a `type-i` value is a 28bit (56 on 64 bit architecture) quantity,
the rest of the bits are used for type information. The larger numerical
types line `type-i32` (on a 32bit platform) is stored in a kind
of encoded pointer/reference to a 32bit value, all of this happens
behind the scenes but it may be important to realize that there is an
extra indirection when operating on such, so-called, boxed values.

### Sequences of operations

LBM has a `progn` special form for evaluation of zero or more expressions
in sequence. This is needed when one wants to execute some operations
for their side-effects, before finally resulting in a value.

Look at the `if` form for example. Here, `condition`, `then-expr` and
`else-expr` each should be a single expression not a sequence of
operations as expected in, for example, a language like C. So if we
want to do more than a single thing, like print a string, redefine a
value, and compute a result, you need to use `progn`

``` lisp
(if condition then-expr else-expr)
```

A `progn` expression has the form `(progn expr1 ... exprN)`
and it can be used anywhere where it is ok to use an expression.


Below is an example that defines a value and prints in the same expression.

```
# (progn (define ape 'monkey) (print ape))
monkey
> t
```
# The special forms (or keywords) of LispBM

The special forms of LBM are a bit like the set of keywords you find
in many languages. The special forms are each identified via a symbol
and they are "applied" in a similar way to functions. That is, if you
want to apply a special form, that special form symbol will be first
in a list. The special part of a special form is that an application
of a special form does **not** need to follow the same behavior as a
function application (where all arguments are evaluated before the
application). There aren't that many special forms, so fortunately not
a lot of "special" behavior to memorize.


| Special form | Description                                                   |
| ---          |  ---                                                          |
| `if`         | For expressing conditionals.                                  |
| `lambda`     | Creates an anonymous function.                                |
| `let`        | Creating local bindings. Support mutually recursive bindings. |
| `define`     | Create a global binding.                                      |
| `progn`      | Execute a sequence of operations.                             |
| `read`       | Parse a string into LBM code or data.                         |
| `match`      | Pattern matching.                                             |
| `macro`      | Create an anonymous macro.                                    |
| `and`        | Boolean and.                                                  |
| `or`         | Boolean or.                                                   |
| `call-cc`    | This will be a chapter of its own some time in the future.    |
| `recv`       | Receive a message.                                            |


If we look at `(if cond-expr then-expr else-expr)` and `(define a 10)`
they look very similar to a function application `(f a b)`. In a
function application the arguments `a` and `b` will be evaluated
before the function is applied. This behavior would be very strange
in the case of `if` where it would mean that the `then-expr` and
`else-expr` are both evaluated. In a definition it would be strange to
evaluate the symbol `a` before creating the binding.

`and` and `or` are special forms to make the short-circuiting behavior
possible.  This is that they will execute only as far as needed to
decide what the output result will be.


# Concluding Chapter 1

This was a quick walk-through of LBM syntax. Just enough to get
started.  In the next Chapter we look at some list-processing (lisp -
LISt Processing).


