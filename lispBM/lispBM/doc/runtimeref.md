# LispBM Runtime Extensions Reference Manual

The runtime extensions, if present, can be either compiled in a minimal or a full mode. In the minimal mode only `set-eval-quota` is present. Minimal mode is the default when compiling LBM. To get the full mode the `-DFULL_RTS_LIB` flag must be used when compiling. 

## Environments


### env-get

`env-get` can be used to reify, turn into value, parts of the global environment. The global environment is stored as a hashtable and an index into this hashtable is used to extract the bindings stored under that hash. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(env-get 0)
```


</td>
<td>

```clj
((chapter-memory section 2 "Memory" ((newline (section 3 "mem-num-free" ((para ("`mem-num-free` returns the number of free words in the LBM memory." "This is the memory where arrays and strings are stored.")) (code ((mem-num-free))) nil)) newline hline) (
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 1)
```


</td>
<td>

```clj
((gc-stack newline (section 3 "set-gc-stack-size" ((para ("With `set-gc-stack-size` you can change the size of the stack used for heap traversal" "by the garbage collector.")) (code ((set-gc-stack-size 100))) nil)) newline hline) (bold closure (str) (list
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 2)
```


</td>
<td>

```clj
((chapter-gc section 2 "GC" ((newline (section 3 "set-gc-stack-size" ((para ("With `set-gc-stack-size` you can change the size of the stack used for heap traversal" "by the garbage collector.")) (code ((set-gc-stack-size 100))) nil)) newline hline))) (pro
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 3)
```


</td>
<td>

```clj
((environment-get newline (section 3 "env-get" ((para ("`env-get` can be used to reify, turn into value, parts of the global environment." "The global environment is stored as a hashtable and an index into this hashtable" "is used to extract the bindings 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 4)
```


</td>
<td>

```clj
((environment-set newline (section 3 "env-set" ((para ("`env-set` destructively sets an entry in the global environment hashtable.")) (program (((if (eq (env-get 1) nil) (env-set 1 (list (quote (a . 75))))) (env-get 1)))) (para ("Note that in the example 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 5)
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
(env-get 6)
```


</td>
<td>

```clj
((local-environment-get newline (section 3 "local-env-get" ((para ("`local-env-get` can be used to reify, turn into value, the local environment.")) (code ((local-env-get))) (program (((let ((a 50)) (local-env-get))))) nil)) newline hline) (png-count . 0)
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 7)
```


</td>
<td>

```clj
((chapter-environments section 2 "Environments" ((newline (section 3 "env-get" ((para ("`env-get` can be used to reify, turn into value, parts of the global environment." "The global environment is stored as a hashtable and an index into this hashtable" "
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 8)
```


</td>
<td>

```clj
((symbol-table-size newline (section 3 "symtab-size" ((para ("`symtab-size` returns the size of the symbol table in bytes.")) (code ((symtab-size))) nil)) newline hline) (code-disp closure (c) (list (quote code-disp) c) nil) (defun macro (name args body) 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 9)
```


</td>
<td>

```clj
((symbol-table-size-flash newline (section 3 "symtab-size-flash" ((para ("`symtab-size-flash` returns the size in bytes of the portion of the symbol table" "that is stored in flash.")) (code ((symtab-size-flash))) nil)) newline hline) (render-code-disp-ta
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 10)
```


</td>
<td>

```clj
((symbol-table-size-names newline (section 3 "symtab-size-names" ((para ("`symtab-size-names` returns the size in bytes of the string names stored in" "the symbol table.")) (code ((symtab-size-names))) nil)) newline hline) (program-disp closure (c) (list 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 11)
```


</td>
<td>

```clj
((symbol-table-size-names-flash newline (section 3 "symtab-size-names-flash" ((para ("`symtab-size-names` returns the size in bytes of the string names stored in" "the symbol table in flash.")) (code ((symtab-size-names-flash))) nil)) newline hline) (imag
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 12)
```


</td>
<td>

```clj
((chapter-symboltable section 2 "Symbol table" ((newline (section 3 "symtab-size" ((para ("`symtab-size` returns the size of the symbol table in bytes.")) (code ((symtab-size))) nil)) newline hline) (newline (section 3 "symtab-size-flash" ((para ("`symtab
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 13)
```


</td>
<td>

```clj
((version newline (section 3 "lbm-version" ((para ("`lbm-version` returns the version of the lbm runtime system.")) (code ((lbm-version))) nil)) newline hline) (intersperse closure (str strs) (match strs (((? s)) s) (((? s) ? ss) (str-merge s str (intersp
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 14)
```


</td>
<td>

```clj
((arch newline (section 3 "is-64bit" ((para ("`is-64bit` returns true if a 64bit version of lbm is running.")) (code ((is-64bit))) nil)) newline hline) (image-pair closure (cap0 txt0 fig0 cap1 txt1 fig1) (list (quote image-pair) cap0 txt0 fig0 cap1 txt1 f
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 15)
```


</td>
<td>

```clj
((word newline (section 3 "word-size" ((para ("`word-size` returns 4 on 32bit LBM  and 8 on 64bits.")) (code ((word-size))) nil)) newline hline) (info para ("This document was generated by LispBM version 0.26.0")) (png-file closure nil (progn (var n png-c
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 16)
```


</td>
<td>

```clj
((chapter-versioning section 2 "Version" ((newline (section 3 "lbm-version" ((para ("`lbm-version` returns the version of the lbm runtime system.")) (code ((lbm-version))) nil)) newline hline) (newline (section 3 "is-64bit" ((para ("`is-64bit` returns tru
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 17)
```


</td>
<td>

```clj
((manual (section 1 "LispBM Runtime Extensions Reference Manual" ((para ("The runtime extensions, if present, can be either compiled" "in a minimal or a full mode." "In the minimal mode only `set-eval-quota` is present." "Minimal mode is the default when 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 18)
```


</td>
<td>

```clj
((render-manual closure nil (let ((h (fopen "runtimeref.md" "w")) (r (lambda (s) (fwrite-str h s)))) (progn (gc) (var t0 (systime)) (render r manual) (print "Runtime reference manual was generated in " (secs-since t0) " seconds"))) nil) (tableize closure 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 19)
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
(env-get 20)
```


</td>
<td>

```clj
((to-dot closure (x) (str-merge "digraph SExpression {\n" "   node [shape=ellipse, fontsize=12];\n" "   edge [fontsize=10];\n" (car (cdr (dot-it 1u64 x))) "\n}") nil) (str-merge-list closure (strs) (match strs (nil [0]) (((? s) ? ss) (str-merge s (str-mer
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 21)
```


</td>
<td>

```clj
((ind-spaces closure (n) (str-replicate n 32b) nil) (s-exp-graph closure (img-name code) (list (quote s-exp-graph) img-name code) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 22)
```


</td>
<td>

```clj
((str-merge closure nil (str-join (rest-args)) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 23)
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
(env-get 24)
```


</td>
<td>

```clj
((end))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 25)
```


</td>
<td>

```clj
((render-it closure (rend ss) (match ss (nil (rend "\n")) ((section (? i) (? x) (? xs)) (progn (match i (1 (rend (str-merge "# " x "\n\n"))) (2 (rend (str-merge "## " x "\n\n"))) (3 (rend (str-merge "### " x "\n\n"))) (4 (rend (str-merge "#### " x "\n\n")
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 26)
```


</td>
<td>

```clj
((evaluation-quota newline (section 3 "set-eval-quota" ((para ("`set-eval-quota` sets the number of evaluation steps that is" "given to each context when given turn to execute by the round-robin" "scheduler.")) (code ((set-eval-quota 30))) nil)) newline h
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 27)
```


</td>
<td>

```clj
((chapter-scheduling section 2 "Scheduling" ((newline (section 3 "set-eval-quota" ((para ("`set-eval-quota` sets the number of evaluation steps that is" "given to each context when given turn to execute by the round-robin" "scheduler.")) (code ((set-eval-
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 28)
```


</td>
<td>

```clj
((num-free newline (section 3 "mem-num-free" ((para ("`mem-num-free` returns the number of free words in the LBM memory." "This is the memory where arrays and strings are stored.")) (code ((mem-num-free))) nil)) newline hline) (render closure (rend ss) (m
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 29)
```


</td>
<td>

```clj
((longest-free newline (section 3 "mem-longest-free" ((para ("`mem-longest-free` returns the length in words of the longest" "consecutive sequence of free words in the LBM memory.")) (code ((mem-num-free))) nil)) newline hline) (para closure (str) (list (
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 30)
```


</td>
<td>

```clj
((memory-size newline (section 3 "mem-size" ((para ("`mem-size` returns the size of the LBM memory.")) (code ((mem-size))) nil)) newline hline) (verb closure (str) (list (quote verb) str) nil) (pretty-list closure (c) (match c (nil [0]) (((? x)) (str-merg
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 31)
```


</td>
<td>

```clj
((heap-state newline (section 3 "lbm-heap-state" ((para ("`lbm-heap-state` can be used to query information about heap usage.")) (code ((lbm-heap-state (quote get-heap-size)) (lbm-heap-state (quote get-heap-bytes)) (lbm-heap-state (quote get-num-alloc-cel
```


</td>
</tr>
</table>




---


### env-set

`env-set` destructively sets an entry in the global environment hashtable. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (if (eq (env-get 1) nil) (env-set 1 (list '(a . 75))))
 (env-get 1)
```


</td>
<td>


```clj
((gc-stack newline (section 3 "set-gc-stack-size" ((para ("With `set-gc-stack-size` you can change the size of the stack used for heap traversal" "by the garbage collector.")) (code ((set-gc-stack-size 100))) nil)) newline hline) (bold closure (str) (list
```


</td>
</tr>
</table>

Note that in the example code above there is no guarantee that the symbol `a` actually hashes to index 1 in the environment table. So `a` is most likely impossible to look up from this environment. The use case for `env-set` and `env-get` are rather that they are together.  Use `env-get` to extract index `i` from the table, then modify it in some way and end by using `env-set` to the same index `i`. 




---


### local-env-get

`local-env-get` can be used to reify, turn into value, the local environment. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(local-env-get)
```


</td>
<td>

```clj
nil
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
 (let ((a 50))
     (local-env-get))
```


</td>
<td>


```clj
((a . 50))
```


</td>
</tr>
</table>




---

## GC


### set-gc-stack-size

With `set-gc-stack-size` you can change the size of the stack used for heap traversal by the garbage collector. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(set-gc-stack-size 100)
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

## Memory


### mem-num-free

`mem-num-free` returns the number of free words in the LBM memory. This is the memory where arrays and strings are stored. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(mem-num-free)
```


</td>
<td>

```clj
255094
```


</td>
</tr>
</table>




---


### mem-longest-free

`mem-longest-free` returns the length in words of the longest consecutive sequence of free words in the LBM memory. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(mem-num-free)
```


</td>
<td>

```clj
255024
```


</td>
</tr>
</table>




---


### mem-size

`mem-size` returns the size of the LBM memory. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(mem-size)
```


</td>
<td>

```clj
262144
```


</td>
</tr>
</table>




---


### lbm-heap-state

`lbm-heap-state` can be used to query information about heap usage. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-heap-size)
```


</td>
<td>

```clj
10000000u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-heap-bytes)
```


</td>
<td>

```clj
80000000u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-num-alloc-cells)
```


</td>
<td>

```clj
9926u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-num-alloc-arrays)
```


</td>
<td>

```clj
777u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num)
```


</td>
<td>

```clj
1u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-marked)
```


</td>
<td>

```clj
3131u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-recovered-cells)
```


</td>
<td>

```clj
9996869u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-recovered-arrays)
```


</td>
<td>

```clj
0u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-least-free)
```


</td>
<td>

```clj
9996869u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-last-free)
```


</td>
<td>

```clj
9996869u
```


</td>
</tr>
</table>




---

## Scheduling


### set-eval-quota

`set-eval-quota` sets the number of evaluation steps that is given to each context when given turn to execute by the round-robin scheduler. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(set-eval-quota 30)
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

## Symbol table


### symtab-size

`symtab-size` returns the size of the symbol table in bytes. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(symtab-size)
```


</td>
<td>

```clj
3034u
```


</td>
</tr>
</table>




---


### symtab-size-flash

`symtab-size-flash` returns the size in bytes of the portion of the symbol table that is stored in flash. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(symtab-size-flash)
```


</td>
<td>

```clj
0u
```


</td>
</tr>
</table>




---


### symtab-size-names

`symtab-size-names` returns the size in bytes of the string names stored in the symbol table. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(symtab-size-names)
```


</td>
<td>

```clj
1276u
```


</td>
</tr>
</table>




---


### symtab-size-names-flash

`symtab-size-names` returns the size in bytes of the string names stored in the symbol table in flash. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(symtab-size-names-flash)
```


</td>
<td>

```clj
0u
```


</td>
</tr>
</table>




---

## Version


### lbm-version

`lbm-version` returns the version of the lbm runtime system. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(lbm-version)
```


</td>
<td>

```clj
(0 26 0)
```


</td>
</tr>
</table>




---


### is-64bit

`is-64bit` returns true if a 64bit version of lbm is running. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(is-64bit)
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


### word-size

`word-size` returns 4 on 32bit LBM  and 8 on 64bits. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(word-size)
```


</td>
<td>

```clj
4
```


</td>
</tr>
</table>




---

This document was generated by LispBM version 0.26.0 

