# LispBM library of dynamically loadable functionality

With LispBM comes a small library of dynamically loadable functions and macros. These functions occupy flash memory (if on an embedded platform) but only use heap if they are actually used by the application. Providing dynamically loadable operations is a way to save space while offering more sweet functionality. 

LispBM can be built with varying amount of included dynamic loadable functionality. It is up to the LispBM integrator to decide what is included. As an integrator you can also decide to roll your entirely own set of dynamically loadable operations. 

The inclusion of dynamically loadable functionality from this library is determined when LispBM is compiled using the following flags: 

   - LBM_USE_DYN_FUNS : Add a library of functions to the dynamic loader.
   - LBM_USE_DYN_MACROS : Add a library of macros to the dynamic loader.
   - LBM_USE_DYN_DEFSTRUCT : Add the defstruct mechanism, requires LBM_USE_DYN_FUNS and LBM_USE_DYN_MACROS.
   - LBM_USE_DYN_LOOPS : Add loop macros, requires LBM_USE_DYN_MACROS.
   - LBM_USE_DYN_ARRAYS : Add functions on arrays. Requires LBM_USE_DYN_MACROS and LBM_USE_DYN_LOOPS.

The flags should be given to the compiler as -Dx for example -DLBM_USE_DYN_FUNS. 

## functions


---


### abs

Compute the absolute value 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(abs -1)
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(abs -1.000000f32)
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(abs 1)
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(abs 1.000000f32)
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(abs (sin -3.140000f32))
```


</td>
<td>

```clj
0.001593f32
```


</td>
</tr>
</table>




---


### apply

Apply a function taking n arguments to a list of n elements 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(apply + (list 1 2 3))
```


</td>
<td>

```clj
6
```


</td>
</tr>
</table>




---


### filter

Filter a list from unwanted elements as decided by a predicate 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define even (lambda (x) (= (mod x 2) 0)))
```


</td>
<td>

```clj
(closure (x) (= (mod x 2) 0) nil)
```


</td>
</tr>
<tr>
<td>

```clj
(filter even (list 1 2 3 4 5 6 7 8 9))
```


</td>
<td>

```clj
(2 4 6 8)
```


</td>
</tr>
</table>




---


### foldl

`foldl` walks through a list, left to right, while accumulating a result from applying a function to the element at hand and the result of its previous step. foldl takes an initial value used as the that is combined with the leftmost value of the list in the first iteration. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(foldl + 0 (list 1 2 3 4 5 6 7 8 9 10))
```


</td>
<td>

```clj
55
```


</td>
</tr>
</table>

`foldl` has an advantage over `foldr` in that it is implemented in a tail-recursive and constant-storage way. 

Funnily enough, `foldl` using function `cons` and initial value `nil` converts a list to a snoc-list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(foldl cons nil (list 1 2 3 4 5 6 7 8 9 10))
```


</td>
<td>

```clj
((((((((((nil . 1) . 2) . 3) . 4) . 5) . 6) . 7) . 8) . 9) . 10)
```


</td>
</tr>
</table>

Now we are going off on a tangent, but `car` and `cdr` switches roles with each other when operating on a snoc-list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define my-snoc-list (foldl cons nil (list 1 2 3 4 5 6 7 8 9 10)))
```


</td>
<td>

```clj
((((((((((nil . 1) . 2) . 3) . 4) . 5) . 6) . 7) . 8) . 9) . 10)
```


</td>
</tr>
<tr>
<td>

```clj
(cdr my-snoc-list)
```


</td>
<td>

```clj
10
```


</td>
</tr>
<tr>
<td>

```clj
(car my-snoc-list)
```


</td>
<td>

```clj
(((((((((nil . 1) . 2) . 3) . 4) . 5) . 6) . 7) . 8) . 9)
```


</td>
</tr>
</table>




---


### foldr

`foldr` walks through a list, right to left, while combining value and prev result step by step. An initial value is provided and here used in the first, rightmost, operation. 

`foldr` has a disadvantage compared to `foldl` as I don't think it is possible to give `foldr` a constant-space and tail-recursive implementation. One can make `foldr` tail-recursive by writing it in continuation passing style (CPS). The CPS version of foldr will run without building upon the return stack, but instead it will construct  and expand upon a continuation object each iteration. This continuation object will grow at the same rate as the call-stack otherwise would but it would grow in heap usage. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(foldr + 0 (list 1 2 3 4 5 6 7 8 9 10))
```


</td>
<td>

```clj
55
```


</td>
</tr>
</table>

Much less amusingly compared to `foldl`, `foldr` of `cons` with initial  value  `nil` is the identity function on proper lists. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(foldr cons nil (list 1 2 3 4 5 6 7 8 9 10))
```


</td>
<td>

```clj
(1 2 3 4 5 6 7 8 9 10)
```


</td>
</tr>
</table>




---


### iota

`iota` takes one number as argument and generates a list of values up to (not including) the given number. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(iota 4)
```


</td>
<td>

```clj
(0 1 2 3)
```


</td>
</tr>
</table>




---


### second

`second` extracts the second element from a list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(second (list 1 2 3 4))
```


</td>
<td>

```clj
2
```


</td>
</tr>
<tr>
<td>

```clj
(second (list 1))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---


### str-cmp-asc

compare strings according to alphabetical order ascending. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-cmp-asc "apa" "bepa")
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(str-cmp-asc "bepa" "apa")
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---


### str-cmp-dsc

compare strings according to alphabetical order descending. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-cmp-dsc "apa" "bepa")
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(str-cmp-dsc "bepa" "apa")
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### str-merge

`str-merge` is an alternative name for the `str-join` operations. It is kept around for backwards compatibility. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-merge "Kurt" " " "Russel")
```


</td>
<td>

```clj
Kurt Russel
```


</td>
</tr>
</table>




---


### third

`third` extracts the third element from a list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(third (list 1 2 3 4))
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
(third (list 1))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---

## macros


---


### defun

`defun` is a macro that provides a shorthand form for defining a named function. `(defun name args body)` is expanded into `(define name (lambda args body))`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(defun f (x) (+ x 1))
```


</td>
<td>

```clj
(closure (x) (+ x 1) nil)
```


</td>
</tr>
<tr>
<td>

```clj
(f 1)
```


</td>
<td>

```clj
2
```


</td>
</tr>
</table>




---


### defunret

`defunret` is like `defun` but you are allowed to `return` at any point in the function body. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(defunret g (x) (progn 
    1
    2
    (return 55)
    3
    4
    x))
```


</td>
<td>

```clj
(closure (x) (call-cc-unsafe (lambda (return) (progn 1 2 (return 55) 3 4 x))) nil)
```


</td>
</tr>
<tr>
<td>

```clj
(g 10)
```


</td>
<td>

```clj
55
```


</td>
</tr>
</table>




---


### defmacro

`defmacro` is a macro that provides a shorthand form for defining macros. `(defmacro name args body)` expands into `(define name (macro args body))`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(defmacro my-macro (a) `(list 'apa ,a))
```


</td>
<td>

```clj
(macro (a) (append (quote (list)) (list (quote (quote apa))) (list a)))
```


</td>
</tr>
<tr>
<td>

```clj
(my-macro 10)
```


</td>
<td>

```clj
(apa 10)
```


</td>
</tr>
</table>




---

## loop macros


---


### loopfor

`loopfor` has the form `(loopfor it start cond update body)` and implements a for loop as familiar from for example C. 

`it` is the iterator, `start` is what it is initialized to, `cond` is the condition that has the be true for the loop to continue running, `update` is how to update the iterator after each iteration and body is the code to execute each iteration. The iterator can be accessed from within body. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define r 0)
 (loopfor i 0 (< i 5) (+ i 1)
      (setq r (+ r i)))
 r
```


</td>
<td>


```clj
10
```


</td>
</tr>
</table>




---


### loopwhile

`loopwhile` has the form `(loopwhile cond body)` and implements a while loop as familiar from for example C. 

`cond` is the condition that has the be true for the loop to continue running and `body` is the code to execute each iteration. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define a 0)
 (loopwhile (< a 10)
      (setq a (+ a 1)))
 a
```


</td>
<td>


```clj
10
```


</td>
</tr>
</table>




---


### looprange

`looprange` has the form `(looprange it start end body)` and implements a loop over a range similar to python's `for i in range(n)`. 

Iterate `it` from `start` to `end` and evaluate `body` for each iteration. The iterator it can be accessed from within body. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define b 0)
 (looprange i 0 11 (setq b (+ b i)))
 b
```


</td>
<td>


```clj
55
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define my-ls nil)
 (looprange i 0 5 (setq my-ls (cons i my-ls)))
 my-ls
```


</td>
<td>


```clj
(4 3 2 1 0)
```


</td>
</tr>
</table>




---


### loopforeach

`loopforeach` has the form `(loopforeach it lst body)` and implements a loop over the elements of a list similar to python's `for e in ...`. 

Iterate over every element in the list `lst` and evaluate `body` for each iteration. The iterator `it` can be accessed from within body. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define m 0)
 (loopforeach e (list 2 4 6 8) (setq m (+ m e)))
 m
```


</td>
<td>


```clj
20
```


</td>
</tr>
</table>




---


### loopwhile-thd

`loopwhile-thd` is like `loopwhile` but the thread runs in its own thread asynchronously. The form of a `loopwhile-thd` is `(loopwhile-thd stack cond body)`. 

A  While-loop that starts in a new thread. The argument `stack` is the stack-size of the thread, `cond` is the condition that has the be true for the loop to continue running and `body` is the code to execute each iteration. The difference from the regular loopwhile is that the evaluator will continue running the code after this one before this one finishes, as this loop is evaluated in a new thread. 

The following examples assumes that your LispBM integration has a way to `print`. 

Example that forever prints "Hello World" every two seconds: 

```
 (loopwhile-thd 100 t { 
    (print "Hello World") 
    (sleep 2) 
 })
 ```

 The above is equivalent to the following code 

 ```
 (spawn 100 (fn () (loopwhile t {
                  (print "Hello World")
                  (sleep 2)
 })))
 ```

 It is possible to give the thread a name and/or a stack size. That gives the following combinations of possibilities: 

 No name and default stack size
 

 ```
 (loopwhile-thd () t {
         (print "Hello World1")
         (sleep 2)
 })
 ```

 No name and stack size 100
 

 ```
 (loopwhile-thd 100 t {
         (print "Hello World2")
         (sleep 2)
 })
 ```

 Name ThdTest and default stack size
 

 ```
 (loopwhile-thd "ThdTest" t {
         (print "Hello World3"
)         (sleep 2)
 })
 ```
 Name ThdTest2 and stack size 100
 

 ```
 (loopwhile-thd ("ThdTest2" 100) t {
         (print "Hello World4")
         (sleep 2)
 })
 ```
 




---

## Array functions


---


### list-to-array

Convert a list to an array 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(list-to-array (list 1 2 3))
```


</td>
<td>

```clj
[|1 2 3|]
```


</td>
</tr>
<tr>
<td>

```clj
(list-to-array '(nil nil nil))
```


</td>
<td>

```clj
[|nil nil nil|]
```


</td>
</tr>
</table>




---


### array-to-list

Convert an array to a list 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(array-to-list (list-to-array (list 1 2 3)))
```


</td>
<td>

```clj
(1 2 3)
```


</td>
</tr>
<tr>
<td>

```clj
(array-to-list [|1 2 3 4|])
```


</td>
<td>

```clj
(1 2 3 4)
```


</td>
</tr>
</table>




---


### array?

Array predicate is true for arrays. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(array? [|1 2 3|])
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(array? 1)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(array? 'apa)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(array? (list 1 2 3))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---

## defstruct and its operations


---

`defstruct` defines a datastructure with named fields similar to a `struct` in C.  `defstruct` takes two arguments, a struct name and a list of fields `(defstruct name list-of-fields)`. 

Structs are implemented as arrays of lisp values and offer constant time lookup of each of its fields. The struct itself does not occupy heap cells, but the values stored in the fields may. 

As structs are allocated from array memory (lbm_memory), there is a potential for causing memory fragmentation. 

The example below creates a structure type called my-struct with three fields called `a`, `b` and `c`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(defstruct my-struct (a b c))
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>

Now instances of `my-struct` can be creted using `make-my-struct`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define s1 (make-my-struct))
```


</td>
<td>

```clj
[|my-struct nil nil nil|]
```


</td>
</tr>
</table>

when a struct is defined using `defstruct` a number of functions for manipulation of that kind of struct is automatically generated. 

   - make-name : function for creation of struct with name `name`.
   - name? : predicate that is true for instances of the struct named `name`.
   - name-x : setter/getter for struct `name` and field `x`.

This will be more clear by showing with `my-struct` as example. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(my-struct? s1)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(my-struct? "hej")
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(my-struct? 10)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>

`my-struct?` is a predicate that is true for instances of `my.struct`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(my-struct-a s1 10)
```


</td>
<td>

```clj
[|my-struct 10 nil nil|]
```


</td>
</tr>
<tr>
<td>

```clj
(my-struct-b s1 20)
```


</td>
<td>

```clj
[|my-struct 10 20 nil|]
```


</td>
</tr>
<tr>
<td>

```clj
(my-struct-c s1 30)
```


</td>
<td>

```clj
[|my-struct 10 20 30|]
```


</td>
</tr>
</table>

`my-struct-x` with 2 arguments is a setter. with just the struct instance as argument it is a getter. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(my-struct-a s1)
```


</td>
<td>

```clj
10
```


</td>
</tr>
<tr>
<td>

```clj
(my-struct-b s1)
```


</td>
<td>

```clj
20
```


</td>
</tr>
<tr>
<td>

```clj
(my-struct-c s1)
```


</td>
<td>

```clj
30
```


</td>
</tr>
</table>

Instances of a struct can also be allocated in a compactible memory region (defrag mem). 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define dm (dm-create 1000))
```


</td>
<td>

```clj
DM
```


</td>
</tr>
<tr>
<td>

```clj
(define s2 (make-my-struct dm))
```


</td>
<td>

```clj
[|my-struct nil nil nil|]
```


</td>
</tr>
</table>

For more information about defragmentable memory see the LispBM [reference manual](https://github.com/svenssonjoel/lispBM/blob/master/doc/lbmref.md). 

This document was generated by LispBM version 0.30.3 

