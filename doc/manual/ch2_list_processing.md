
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

The shortest list possible has zero elements and in LBM we use `nil` to
represent this list (`nil` is used for a lot of things in LBM). Longer
lists are constructed from cons-cells that are allocated from the heap.

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
loading: (first (list 1 2 3))
started ctx: 144
<< Context 144 finished with value 1 >>
stack max:  13
stack size: 256
stack sp:   0
```

Example of taking the rest of a list:
```
# (rest (list 1 2 3))
loading: (rest (list 1 2 3))
started ctx: 144
<< Context 144 finished with value (2 3) >>
stack max:  13
stack size: 256
stack sp:   0
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
loading: (ix (list 1 2 3) 2)
started ctx: 156
<< Context 156 finished with value 3 >>
stack max:  13
stack size: 256
stack sp:   0
```

And if you index out of bounds you get the following
```
# (ix (list 1 2 3) 5)
loading: (ix (list 1 2 3) 5)
started ctx: 156
<< Context 156 finished with value nil >>
stack max:  13
stack size: 256
stack sp:   0
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
| `lookup`   | Looks up a value in a list of key-value bindings. |
| `foldr`    | Reduces a list to a single element by application of two-input one output function (from the right). |
| `foldl`    | Similar to above but from the left (beginning). |

We are being a bit vague about the inner workings of these functions here
as the plan is to implement each of them in the next section.

---
**NOTE** 

The `nil` at the end of a list is a convention. There is nothing 
in LBM that enforce that you put `nil` as a termination of a list 
and you could just as well put for example `'cactus` at the end of 
your lists. All your functions on these `'cactus` lists would have to 
be written with that in mind though. So let's stick to convention. 

Another thing to note about cons-cells is that you do not need 
to arrange then into rightwards-nested lists. You could do the oposite. 
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
the `expr` is executed. If the value does not match, the next pattern is checkend 
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
         ( (e . _) e )
         ( (_ . (? xs)) (elem-pm xs e) )))
```
The code above has the same `( nil nil )` base-case but then it 
checks for a list with a first element that is exactly `e` (the value we look for) 
`( (e . _) e)`. The underscore, `_`, "anything" and does not bind that anything to 
any name. We can use the underscore in this case because if the first element in the 
list is `e`, then we are done and dont need to search through the rest. 

The last case, `( (_ . (? xs)) (elem-pm xs e) )` binds the rest of the
list and performs the recursion. The pattern matching cases are tested 
in order from the top, so in this last case we know that the first element
cannot possibly be `e`.



## Conclusion
