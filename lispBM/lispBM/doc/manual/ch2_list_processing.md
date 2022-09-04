
# Chapter 2: List processing

This chapter is all about getting familiar with lists. Lists are
of course a very important concept to get quite well familiarized with
as in Lisp we express our code and data using this construct.

In this chapter the programs will start to become slightly larger and
will be written into files and loaded into the REPL. All examples are
available [here](./ch2_examples).


## Getting started with data in lists

Lists in LBM are build from so-called cons-cells with a `car` and a `cdr`
field. The `car` and `cdr` fields are both large enough to hold a pointer
on the architecture we compile for, so 32bits on 32bit architectures and
64bits on 64bit architectures. The LBM heap is a collection of cons-cells
that you can use build up structures such as lists.

![cons cell](../images/cons_cell.png?raw=true "cons cell")

The shortest list possible has zero elements and in LBM we use `nil` to
represent this list (`nil` is used for a lot of things in LBM). Longer
lists are constructed from cons-cells that are allocated from the heap.

![list](../images/list.png?raw=true "pair")

There are many different ways to build a list in LBM, the most basic way
is to build it manually using `cons` and `nil`. `cons` is a function
that takes two arguments, allocates a cons-cell from the heap and sticks
argument one in the `car` field and argument two in the `cdr`.

Example:
```lisp
(cons 1 nil)
```
Creates a one element list with the value 1 in it by allocating a cons-cell
and sticking `1` in the `car` field and `nil` in the `cdr` field.

Example:
```lisp
(cons 1 (cons 2 nil))
```
Creates a two element list with the values 1 and 2. Here we are creating a
cons-cell with `1` in the `car` and in the `cdr` there is another cons-cell
holding `2` in `car` and `nil` in its `cdr`.

The expression `(cons 1 (cons 2 nil))` has the same result as the
expression `(list 1 2)` but the first form is more telling about exactly how
lists are constructed and in the case of LBM also how they are represented
on the heap as cons-cells. There are other expressions that result
in the same list as well, `'(1 2)` and `` `(1 2)``.

There are built in functions to access the first element of a list
and the "rest" of the list. These functions are traditionally called
`car` and `cdr` in lisps as what they do is exactly accessing these fields
of a cons-cell. In LBM we have the `car` and `cdr` functions but also
names which are a bit more telling and not carrying the same historical baggage.
These equivalent functions are called `first` and `rest`.

Example of taking the first element from a list:
```
# (first (list 1 2 3))
> 1
```

Example of taking the rest of a list:

```
# (rest (list 1 2 3))
> (2 3)
```

So applying `first` (or equivalently `car`) to the list `(list 1 2 3)` gives
back the result 1. Applying `rest` (or if you prefer `cdr`) to the same list
produces a result that the REPL prints as `(2 3)`.

LBM provides one more built in function on lists, called `ix` for accessing
an arbitrary element of a list. `ix` takes one list and one index as arguments
and returns the value at that position in the list. if the index is too large
(outside of the list), the result will be `nil`.

Example of indexing into a list:
```
# (ix (list 1 2 3) 2)
> 3
```

And if you index out of bounds you get the following
```
# (ix (list 1 2 3) 5)
> nil
```

The REPL contains a small library of list functions that will be loaded
dynamically if you try to use them. The list below shows what functionality
these provide.

| Function   | Description                         |
| ---        | ---                                 |
| `reverse`  | Reverses a list.                    |
| `iota`     | Creates a list enumerating a range. |
| `length`   | Calculates the length of a list.    |
| `take`     | Creates a list containing some number of elements taken from the input list. |
| `drop`     | Creates a list by removing some number of elements from a list. |
| `zip`      | Produces a list of two-element lists from two input lists. |
| `map`      | Produces a list that contains the results of applying a function to all elements in an input list. |
| `foldr`    | Reduces a list to a single element by application of two-input one output function (from the right). |
| `foldl`    | Similar to above but from the left (beginning). |

We are being a bit vague about the inner workings of these functions here
as the plan is to implement each of them in the next section.

---
**NOTE**

The `nil` at the end of a list is a convention. There is nothing in
LBM that enforce that you put `nil` as a termination of a list and you
could just as well put for example `'cactus` at the end of your
lists. All your functions on these `'cactus` lists would have to be
written with that in mind though. So let's stick to convention. Built
in functions on lists will expect `nil` as the terminator.


Another thing to note about cons-cells is that you do not need to
arrange then into rightwards-nested lists. You could do the opposite.
You can also build trees by allowing cons-cells in both the `car` and
`cdr` fields. This is entirely up to the programmer.

---

## Writing functions on lists

The linked-lists used in LBM are an example of a recursive data-structure,
that is, all lists of 1 or more elements are built up from smaller lists.
Operating on such a structure is often very cleanly expressible as a
recursive function, which is something we will implement lots of in
this section. When writing recursive programs on list these will follow
a general pattern, there will be one terminating base-case for the empty list `nil`
and there will be one case for a list composed of an element and a smaller
list.

The first example is a function that finds out if a particular value
occurs in a list. We call this function `elem` and it takes one list
and one value as input, if the value is in the list the result is some "true"
value and otherwise it is `nil`. Since any value other than `nil` is "true"
we can return the value we search for as the result.

---
**NOTE**

The REPL currently does not support entering multi-line programs.
Write the programs here into a file and load that file into the
REPL using the `:load` command.

The examples here are available in `listcode.lisp` in the
`ch2_examples` directory ([here](./ch2_examples/listcode.lisp)).

if you start the REPL from the `ch2_examples` directory, you
can load the list examples using the command:

```
# :load listcode.lisp
```

---


```lisp
(defun elem (ls e)
  (if (eq ls 'nil)
      'nil
      (if (eq (first ls) e)
          e
          (elem (rest ls) e))))
```
The code above implements the `elem` function using conditionals.
The first conditional checks if the list is empty `(eq ls 'nil)`. If
this condition is true we have searched through the entire list and found
no matching value. If the list is not empty, we check if the first element
in the list, `first ls` is the value we are looking for and if it is
we return it (meaning true). If the first element is not the value we look
for, we recursively check if the value is in the rest of the list.

Many nested conditionals make code a bit right-leaning and the above
program would be better expressed using some kind of case-splitting.
In LBM the case-splitting construct is called `match`.

```lisp
(defun elem-pm (ls e)
  (match ls
         ( nil nil )
         ( ((? x) . (? xs))
           (if (eq x e) e (elem-pm xs e)) )))
```

The code above case splits on `ls` into two different checks,
The first for the empty list `( nil nil )`. The `(nil nil)`
is a pattern matching expression of the form `( pattern expr )`
and if the value matched upon (in this case `ls`) "matches" the `pattern`,
the `expr` is executed. If the value does not match, the next pattern is checked 
and if there is no pattern that matches LBM stops evaluating the program and
returns a `no_match` error. This means that pattern matches must be
in some sense "complete", there must be a pattern to catch each possible
value you apply the function to.

The next pattern is `((? x) . (? xs))` and this pattern matches
any cons-cell and names the first `x` and the rest `xs`. The syntax `(? x)`
means "match any value and bind that value to the name `x`". So this pattern
will match on any list with 1 or more elements and the `x` will be bound to the
first element of the list while `xs` is bound to the rest of the list.
The `expr` in this pattern case is `(if (eq x e) e (elem-pm xs e))`
and it checks if the `x` is the value we look for, then we are done, or
if we should do recursion over `xs`.

You can express the same thing a bit more compactly by splitting up
into three patterns instead and being a bit more precise in each.

```lisp
(defun elem-pm2 (ls e)
    (match ls
           ( nil nil )
           ( (,e . _) e )
           ( (_ . (? xs)) (elem-pm2 xs e) )))
```

The code above has the
same `( nil nil )` base-case but then it checks for a list with a
first element that is exactly `e` (the value we look for) `( (,e . _)
e)`. The `,e` syntax means that the value of `e` is the pattern, not
the symbol `e` itself. The underscore, `_`, means "anything" and does
not bind that anything to any name. We can use the underscore in this
case because if the first element in the list is `e`, then we are done
and don't need to search through the rest.

The last case, `( (_ . (? xs)) (elem-pm xs e) )` binds the rest of the
list and performs the recursion. The pattern matching cases are tested
in order from the top, so in this last case we know that the first element
cannot possibly be `e`.

### The importance of tail-recursion

When writing recursive functions one must be careful not to exhaust 
all stack. It is possible to write reclusive functions that evaluate 
in constant space (not growing linear with number of calls) and these
recursive functions are called tail-recursive.

What tail-recursion and non-tail-recursion looks like is best illustrated by
examples. The example below implements the `length` function on a list
and does so in a non-tail-recursive manner:

```lisp
(defun length-notail (ls)
  (if (eq ls nil)
      0
    ( + 1 (length-notail (cdr ls)))))
```

The `length-notail` function expects that the input is a proper list and splits
into two cases, one for the empty list and one for a list containing at least 1
element. The empty list is of length 0, a list longer than 0 elements is 1 + the length
of the rest of the list.

Running this program on a list produces the following:

```
# (length-notail (list 1 2 3 4 5))
> 5
```

So, what is the problem? The problem is this expression containing the
recursive call `( + 1 (length-notail (cdr ls)))`. There is a `+ 1` on
the "outside" of the recursive call. If you evaluate `(a (b ..))` then
`a` is supposed to be applied to the result of `b` which means that we
must remember that until the `b` returns. In the LBM implementation
`a` is turned into a so-called "continuation" that occupies some
amount of space on the runtime stack.  If the recursion is "deep", the
list we count is long, then this stack space usage will grow
proportional to the length of the list. That is very bad:

```
# (length-notail (iota 1024))
***	Error: out_of_stack

> out_of_stack
```

The list `(iota 1024)` is 1024 elements long! And while recurring over
it, we exhaust all stack!

A common pattern to apply that removes the "outside" function
application around the recursive call is to have the recursive
function take an extra argument that accumulates partial results
throughout the recursion. This is usually accomplished using a small
helper function taking that extra argument:

```lisp
(defun length-tail (ls)
  (let ((len-helper (lambda (acc ls)
                      (if (eq ls nil)
                          acc
                        (len-helper (+ 1 acc) (rest ls))))))
    (len-helper 0 ls)))
```

The `length-tail` function is implemented using a helper called
`len-helper`.  `len-helper` takes two arguments, a number and a
list. The idea is that we use the number argument to accumulate the
length we have seen so far through the recursion. In each recursive
call we add 1 to the accumulator.

The key here is again the then-branch `(len-helper (+ 1 acc) (rest
ls))`. Note that here is nothing on the "outside" of the recursive
call that we need to remember while executing `len-helper`.

Now its no problem to compute the length of `(iota 1024)`:

```
# (length-tail (iota 1024))
> 1025
```

### Iota and Reverse

The `iota` and the `reverse` functions are both examples building a 
list. `iota` builds a list based an input argument and enumerates all 
numbers from 0 up to and including the number provided as argument. 
`reverse` takes a list as input, deconstructs it and creates a new 
list in the reversed order. 

Examples: 
```
# (iota 10)
> (0 1 2 3 4 5 6 7 8 9)
``` 

``` 
# (reverse (iota 10))
> (9 8 7 6 5 4 3 2 1 0)
``` 

Note that the result of `reverse` is a new list as expected in
functional programming. So if you bind the result of `iota` to a name
and then run reverse on that "variable", future references to that
name will result in the original list.

Example: 
```
# (define my-list (iota 10))
> my-list
# my-list
> (0 1 2 3 4 5 6 7 8 9)
# (reverse my-list)
> (9 8 7 6 5 4 3 2 1 0)
# my-list
> (0 1 2 3 4 5 6 7 8 9)
``` 

Note that calling `(reverse my-list)` returns a new, reversed lists,
while `my-list` remains intact.

`iota` is implemented using the same tail-recursion "trick" as we have
seen earlier. The helper function with the extra argument is called
`iacc` here, for iota-accumulate.

```lisp
(defun iota (n)
  (let ((iacc (lambda (acc i)
                (if (< i 0) acc
                    (iacc (cons i acc) (- i 1))))))
    (iacc nil (- n 1))))
```

`iacc` takes an `acc` parameter in which the result list is accumulated 
and a number `i` which should be 0 or larger. Applied on a negative 
number `iota` returns  `nil`. 


The `reverse` function is very similar to `iota` in spirit. 
Instead of decreasing a number for each recursive call, a smaller 
list is passed as argument. This brings to mind another "rule" to 
apply when writing recursive functions: the argument given to the 
recursive application should be closer to the terminating case. 
Usually this "closer" property means that the argument to the 
recursive call will be either structurally (as in a shorter list) 
or numerically smaller than the argument passed to the current 
"iteration". 


```lisp
(defun reverse (xs)
  (let ((revacc (lambda (acc xs)
                  (if (eq nil xs) acc
                      (revacc (cons (first xs) acc) (rest xs))))))
    (revacc nil xs)))
```
`revacc` splits into two cases, one for the empty list in which the 
accumulated reversed list is returned and one for non-empty lists. 
In the non-empty case an element is removed from the start of the 
input list and placed first in the accumulation-list. 

Imagine having a deck of cards. If you pick a card from the top of
your deck and place that next to the deck on the table. Then repeat,
pick the current top of the card next to the deck. continue repeating
until the entire deck has moved over to the other pile and it is now
in reversed order.

---
**NOTE** 

If you run `iota` with a large argument, the resulting list may end 
up being longer than there are available heap cells. 

``` 
# (iota 4096)
***	Error: out_of_memory

> out_of_memory
```

The REPL starts up with 2048 heap-cells, so it is clearly impossible
to make a list that is 4096 elements long! 

There is a REPL command to increase the size of the heap. For example type
`:heap 8192` and press enter. 

```
# :heap 8192
Array extensions loaded
String extensions loaded
Math extensions loaded
```
This command resets the REPL (all state is cleared) and the heap is resized. 

In this larger heap it is possible to run `(iota 4096)`.

```
# (iota 4096)
> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 ...
``` 

When the REPL prints a long list it will at some point cut off and
indicate with `...` that there are really more elements in the
list. Printing arbitrary recursive structures requires storage, a
printing stack, and the printer is limited to operate within a small
fixed amount of memory.

---

### Take, Drop and Zip

The `take` function takes n elements from the start of a list 
and returns these as a new list. 

The code below is a first attempt at implementing `take` but 
unfortunately it is a non-tail-recursive function!

``` 
(defun take-n (n xs)
  (if ( = 0 n)
      nil
      (cons (first xs) (take-n (- n 1) (rest xs) ))))
```
`take-n` takes a number of elements to take from the list passed 
as the second argument. `n` is decreased in each recursive call (bringing 
us closer to the terminating case where `n` is 0. 

```
# (take-n 5 (iota 10))
> (0 1 2 3 4)
```

The problem with the `take-n` function is that there is `cons` outside
of the recursive call of `take-n` and unfortunately it is not enough
to just directly apply the "accumulator trick" to make this function
tail-recursive.  But let's try it and see what happens!

```
(defun take-t (n xs)
  (let ((takeacc (lambda (acc n xs)
                   (if (= n 0) acc
                       (takeacc (cons (first xs) acc) (- n 1) (rest xs))))))
    (takeacc nil n xs)))
``` 

So, the result of `take-t` comes out in reversed order!

``` 
# (take-t 5 (iota 10))
> (4 3 2 1 0)
``` 

To fix this we can add an application of `reverse` to `take-t` 

``` 
(defun take-t (n xs)
  (let ((takeacc (lambda (acc n xs)
                   (if (= n 0) acc
                       (takeacc (cons (first xs) acc) (- n 1) (rest xs))))))
    (reverse (takeacc nil n xs))))
```

It is unfortunate that in some cases we have to make functions more
expensive when making then tail-recursive.

The `drop` function is, however, very easy to get right. 

```
(defun drop-n (n xs)
  (if ( = 0 n)
      xs
      (drop-n (- n 1) (rest xs))))
``` 

The REPL contains functions with names `drop` and `take` with the 
functionality implemented above. 

The zip function takes two lists and creates a list of pairs of 
elements from the two input arrays. 

Example: 

``` 
# (zip (list 1 2 3 ) (list 'monkey 'zebra 'elephant))
> ((1 . monkey) (2 . zebra) (3 . elephant))
```

The implementation of zip is very similar to take, just that 
here we take all elements and from two arrays at once. Implementing 
`zip` in a non-tail-recursive looks like this: 

```lisp
(defun zip (xs ys)
  (if (or (eq xs nil) (eq ys nil)) nil
      (cons (cons (first xs) (first ys)) (zip (rest xs) (rest ys)))))
```

The tail-recursive version of `zip` is included in the
[listcode.lisp](./ch2_examples/listcode.lisp) file if you are curious.
Otherwise the implementation of tail-recursive `zip` is left as 
an exercise.

### Higher order functions: Map, Foldr and Foldl

A higher-order function is a function that takes another function 
as argument or gives a function back as a result. 

The `map` function is one example of a higher order function. `map`
takes two arguments, a function and a list, it then applies the function 
to each element in the list and builds up a list of all the results. 

Example:

``` 
# (map (lambda (x) (+ x 1)) (list 1 2 3))
> (2 3 4)
``` 

In the example above, the anonymous function `(lambda (x) (+ x 1))` 
is applied to each of the elements of the list `(1 2 3)` resulting 
in the list `(2 3 4)`. It also works with named functions. 

Example: 

``` 
# (defun +1 (x) (+ x 1))
> +1
``` 
The function called `+1` defined above, adds 1 to the argument. 
Now you can `map` this `+1` function over a list if you like: 

```
# (map +1 (list 1 2 3))
> (2 3 4)
```

Let's look at the non-tail-recursive variant of `map`: 

```
(defun map (f xs)
  (if (eq xs nil) nil
      (cons (f (first xs)) (map f (rest xs)))))
```

The `map` function takes arguments `f` and `xs`, representing 
the function to map `f` and the list `xs`. If the list is empty 
return `nil`, otherwise apply `f` to the first element and 
put the result of that first in the new result list. 

Since this implementation of `map` is non-tail-recursive we cannot 
use it on long lists: 

```
# (map +1 (iota 1024))
***	Error: out_of_stack

> out_of_stack
# (map-t +1 (iota 1024))
> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 28...
``` 

The tail-recursive version of `map` is, just like `zip`, included in the
[listcode.lisp](./ch2_examples/listcode.lisp) file if you are curious 
and otherwise left as an exercise. 

`foldr` and `foldl` are two more higher-order functions that 
are used to combine elements of a list using a function. The function 
passed into foldr and foldl takes two arguments compared to the 
single argument of `map`. These function can for example be used 
to reduce a list into a single value by summing up all elements. 

Example: 

``` 
# (foldl + 0 (list 1 2 3 4 5))
> 15
``` 

In the case of using `foldl` as in the example, the list is summed 
up as `(((((0 + 1) + 2) + 3) + 4) + 5)`. The example below 
shows that `foldr` computes the same result. 

```
# (foldr + 0 (list 1 2 3 4 5))
> 15
``` 

But in this case the sum is computed as `(1 + (2 + (3 + (4 + (5 + 0)))))`. 

There is no rule that says that the result of a fold should be a scalar. 
For example: 

```
# (foldr cons nil (list 1 2 3))
> (1 2 3)
# (foldl cons nil (list 1 2 3 ))
> (((nil . 1) . 2) . 3)
``` 

Above we can see that `foldr cons nil` is an identity function on
lists and that `foldl cons nil` converts a list into some reversed
kind of "snoc"-format.

The implementation of `foldr` and `foldl` follow the same pattern as
we have seen many times before now. Interestingly though `foldl` comes
very naturally as a tail-recursive function while `foldr` does not!

```lisp
(defun foldl (f i xs)
  (if (eq xs nil) i
      (foldl f (f i (first xs)) (rest xs))))
```

`foldl` takes three arguments, `f` a function, `i` an identity element, 
and `xs` a list. Here it is very natural to accumulate up the result 
in the `i` parameter and it naturally ends up a tail-recursive function.

```lisp
(defun foldr (f i xs)
  (if (eq xs nil) i
      (f (first xs) (foldr f i (rest xs)))))
```

`foldr` above is more awkward with its application of `f` 
to `(first xs)` and to the result of `(foldr f i (rest xs))` 
which means that there will be a build-up of continuations on the 
stack.


## Association lists

Association lists are meant for maintaining a key-value lookup 
structure. `(list '(1 . horse) '(2 . donkey) '(3 . shark))` 
is an example of an association list that associates keys 1, 2, 3 with 
the animals horse, donkey and shark. You can look up the 
value associated with a key by using the function `assoc`

Example: 
``` 
# (assoc (list '(1 . horse) '(2 . donkey) '(3 . shark)) 2)
> donkey
``` 

So, an association list is just a regular list, you can create it with
`list` but each element in the list must be a pair. Pairs can be
created either as in the example using `.` or they could be created
using cons. `(cons 1 2)` is equivalent to `'(1 . 2)` note the `'`
mark!

there is a three argument version of `cons` called `acons` that is 
meant to make adding associations to an association list (alist) easier. 

``` 
# (acons 4 'lemur (list '(1 . horse) '(2 . donkey) '(3 . shark)))
> ((4 . lemur) (1 . horse) (2 . donkey) (3 . shark))
``` 
`acons` takes the key as first argument, then the value and last the alist 
to append the key-value pair to. Note that `acons` just adds, it doesn't check 
if the association already exists, it will gladly add a new copy of it anyway. 


There is a built in function for destructively updating an alist 
that you have bound to a name, called `setassoc` 

Example: 

```
# (define my-alist (list '(1 . blue) '(2 . orange) '(3 . green)))
> my-alist
# (setassoc my-alist 2 'purple)
> ((1 . blue) (2 . purple) (3 . green))
```

Any future references to `my-alist` will reflect that change. 

```
# my-alist
> ((1 . blue) (2 . purple) (3 . green))
```

There are a bunch of functions like `setassoc` that perform destructive 
updates in LBM. 

If you want a more functional-programming style way to re-associate, that 
is by returning a fresh new alist with the updated field then that is 
quite easy using `map`. 

``` 
(defun replace-assoc (x y)
  (if (eq (first x) (first y))
      x
      y))

(defun reassoc (x xs) 
    (map (lambda (y) (replace-assoc x y)) xs))
``` 

Example 

``` 
# (reassoc '(2 . dung-beetle) (acons 4 'lemur (list '(1 . horse) '(2 . donkey) '(3 . shark))))
> ((4 . lemur) (1 . horse) (2 . dung-beetle) (3 . shark))
```
