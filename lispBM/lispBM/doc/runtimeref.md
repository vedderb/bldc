# LispBM Runtime Extensions Reference Manual

The runtime extensions, if present, can be either compiled in a minimal or a full mode. In the minimal mode only `set-eval-quota` is present. Minimal mode is the default when compiling LBM. To get the full mode the `-DFULL_RTS_LIB` flag must be used when compiling. 

## Errors


### hide-trapped-error

The default behavior is to print error messages even if the error is trapped. Trapped errors can be hidden by calling this function at the beginning of a program. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(hide-trapped-error)
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


### show-trapped-errors

If you have hidden trapped errors they can be toggled back to being showed again using this function. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(show-trapped-error)
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
((gc-stack newline (section 3 "set-gc-stack-size" ((para ("With `set-gc-stack-size` you can change the size of the stack used for heap traversal" "by the garbage collector.")) (code ((set-gc-stack-size 100))) nil)) newline hline) (render-program-disp-gif-p
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
((gc-is-always-gc newline (section 3 "is-always-gc" ((para ("The `is-always-gc` predicate is true if LBM is built with the LBM_ALWAYS_GC debug flag.")) (code ((is-always-gc))) nil)) newline hline) (str-merge-list closure (strings) (match strings (nil [0]) 
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
((chapter-gc section 2 "GC" ((newline (section 3 "set-gc-stack-size" ((para ("With `set-gc-stack-size` you can change the size of the stack used for heap traversal" "by the garbage collector.")) (code ((set-gc-stack-size 100))) nil)) newline hline) (newlin
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
((environment-get newline (section 3 "env-get" ((para ("`env-get` can be used to reify, turn into value, parts of the global environment." "The global environment is stored as a hashtable and an index into this hashtable" "is used to extract the bindings s
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
((environment-set newline (section 3 "env-set" ((para ("`env-set` destructively sets an entry in the global environment hashtable.")) (program (((if (eq (env-get 1) nil) (env-set 1 (list (quote (a . 75))))) (env-get 1)))) (para ("Note that in the example c
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
((render closure (rend ss) (match ss (nil t) (((? x) ? xs) (progn (render-it rend x) (render rend xs))) ((? x) (print "RENDER ERROR: " x))) nil) (code-raw closure (c) (list (quote code-raw) c) nil))
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
((environment-drop newline (section 3 "env-drop" ((para ("drop a binding from an environment.")) (code ((env-drop (quote a) (quote ((a . 10) (b . 20) (c . 30)))))) nil)) newline hline) (pretty-aligned-on-top closure (n cs) (match cs (nil [0]) (((? x) ? xs)
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
((pretty closure (toplevel ind c) (if (and toplevel (list? c)) (pretty-toplevel-list c) (pretty-ind ind c)) nil))
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
((local-environment-get newline (section 3 "local-env-get" ((para ("`local-env-get` can be used to reify, turn into value, the local environment.")) (code ((local-env-get))) (program (((let ((a 50)) (local-env-get))))) nil)) newline hline) (disp-render-mac
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
((global-environment-size newline (section 3 "global-env-size" ((para ("Get the size (in number of bindings) of the global env.")) (code ((global-env-size))) nil)) newline hline) (render-code-png-table closure (rend img colors c) (progn (rend "<table>\n") 
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
((chapter-environments section 2 "Environments" ((newline (section 3 "env-get" ((para ("`env-get` can be used to reify, turn into value, parts of the global environment." "The global environment is stored as a hashtable and an index into this hashtable" "i
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
((symbol-table-size newline (section 3 "symtab-size" ((para ("`symtab-size` returns the size of the symbol table in bytes.")) (code ((symtab-size))) nil)) newline hline) (frame-i . 0))
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
((symbol-table-size-flash newline (section 3 "symtab-size-flash" ((para ("`symtab-size-flash` returns the size in bytes of the portion of the symbol table" "that is stored in flash.")) (code ((symtab-size-flash))) nil)) newline hline) (table closure (heade
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
((symbol-table-size-names newline (section 3 "symtab-size-names" ((para ("`symtab-size-names` returns the size in bytes of the string names stored in" "the symbol table.")) (code ((symtab-size-names))) nil)) newline hline) (render-code-res-raw-pairs closur
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
((symbol-table-size-names-flash newline (section 3 "symtab-size-names-flash" ((para ("`symtab-size-names` returns the size in bytes of the string names stored in" "the symbol table in flash.")) (code ((symtab-size-names-flash))) nil)) newline hline) (rende
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
((chapter-symboltable section 2 "Symbol table" ((newline (section 3 "symtab-size" ((para ("`symtab-size` returns the size of the symbol table in bytes.")) (code ((symtab-size))) nil)) newline hline) (newline (section 3 "symtab-size-flash" ((para ("`symtab-
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
((version newline (section 3 "lbm-version" ((para ("`lbm-version` returns the version of the lbm runtime system.")) (code ((lbm-version))) nil)) newline hline) (code-examples closure (c) (list (quote code-examples) c) nil))
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
((endian newline (section 3 "lbm-endian" ((para ("`lbm-endian` returns the endianness of the system lbm is running on.")) (code ((lbm-endian))) nil)) newline hline) (end) (glob-yeet))
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
((arch newline (section 3 "is-64bit" ((para ("`is-64bit` returns true if a 64bit version of lbm is running.")) (code ((is-64bit))) nil)) newline hline) (s+ closure (s ss) (cons s ss) nil) (eval-animation closure (gif c frames) (progn (setq frame-i 0) (setq
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
((word newline (section 3 "word-size" ((para ("`word-size` returns 4 on 32bit LBM  and 8 on 64bits.")) (code ((word-size))) nil)) newline hline) (ref-entry closure (str strings-or-tags) (let ((tags (and (rest-args 0) strings-or-tags)) (strings (or (rest-ar
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
((chapter-versioning section 2 "Version" ((newline (section 3 "lbm-version" ((para ("`lbm-version` returns the version of the lbm runtime system.")) (code ((lbm-version))) nil)) newline hline) (newline (section 3 "is-64bit" ((para ("`is-64bit` returns true
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
((hide-em newline (section 3 "hide-trapped-error" ((para ("The default behavior is to print error messages even if the error is trapped." "Trapped errors can be hidden by calling this function at the beginning of a program.")) (code ((hide-trapped-error)))
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
((show-em newline (section 3 "show-trapped-errors" ((para ("If you have hidden trapped errors they can be toggled back to being showed again" "using this function.")) (code ((show-trapped-error))) nil)) newline hline) (chapter-scheduling section 2 "Schedul
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
((chapter-errors section 2 "Errors" ((newline (section 3 "hide-trapped-error" ((para ("The default behavior is to print error messages even if the error is trapped." "Trapped errors can be hidden by calling this function at the beginning of a program.")) (
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
((manual (section 1 "LispBM Runtime Extensions Reference Manual" ((para ("The runtime extensions, if present, can be either compiled" "in a minimal or a full mode." "In the minimal mode only `set-eval-quota` is present." "Minimal mode is the default when c
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
((render-manual closure nil (let ((h (fopen "runtimeref.md" "w")) (r (lambda (s) (fwrite-str h s)))) (progn (gc) (var t0 (systime)) (render r manual) (print "Runtime reference manual was generated in " (secs-since t0) " seconds"))) nil) (pretty-toplevel-li
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
((chapter-threads section 2 "Threads" ((newline (section 3 "mailbox-get" ((para ("`mailbox-get` returns the mailbox contents of a thread as a list." "The form of a `mailbox-get` expression is `(mailbox-get pid)`." "Note that `mailbox-get` does **NOT** empt
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
((num-free newline (section 3 "mem-num-free" ((para ("`mem-num-free` returns the number of free words in the LBM memory." "This is the memory where arrays and strings are stored.")) (code ((mem-num-free))) nil)) newline hline) (render-code-res-pairs closur
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
((longest-free newline (section 3 "mem-longest-free" ((para ("`mem-longest-free` returns the length in words of the longest" "consecutive sequence of free words in the LBM memory.")) (code ((mem-num-free))) nil)) newline hline) (info para ("This document w
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
((memory-size newline (section 3 "mem-size" ((para ("`mem-size` returns the size of the LBM memory.")) (code ((mem-size))) nil)) newline hline) (para closure (str) (list (quote para) str) nil) (bullet closure (ss) (verb (map (lambda (x) (str-merge "   - " 
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
((heap-state newline (section 3 "lbm-heap-state" ((para ("`lbm-heap-state` can be used to query information about heap usage.")) (code ((lbm-heap-state (quote get-heap-size)) (lbm-heap-state (quote get-heap-bytes)) (lbm-heap-state (quote get-num-alloc-cell
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
((chapter-memory section 2 "Memory" ((newline (section 3 "mem-num-free" ((para ("`mem-num-free` returns the number of free words in the LBM memory." "This is the memory where arrays and strings are stored.")) (code ((mem-num-free))) nil)) newline hline) (n
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
((gc-is-always-gc newline (section 3 "is-always-gc" ((para ("The `is-always-gc` predicate is true if LBM is built with the LBM_ALWAYS_GC debug flag.")) (code ((is-always-gc))) nil)) newline hline) (str-merge-list closure (strings) (match strings (nil [0]) 
```


</td>
</tr>
</table>

Note that in the example code above there is no guarantee that the symbol `a` actually hashes to index 1 in the environment table. So `a` is most likely impossible to look up from this environment. The use case for `env-set` and `env-get` are rather that they are together.  Use `env-get` to extract index `i` from the table, then modify it in some way and end by using `env-set` to the same index `i`. 




---


### env-drop

drop a binding from an environment. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(env-drop 'a '((a . 10) (b . 20) (c . 30)))
```


</td>
<td>

```clj
((b . 20) (c . 30))
```


</td>
</tr>
</table>




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
((a . 50) (res-str . $placeholder) (res (a . 50) (res-str . $placeholder) (res (a . 50) (res-str . $placeholder) (res (a . 50) (res-str . $placeholder) (res (a . 50) (res-str . $placeholder) (res (a . 50) (res-str . $placeholder) (res (a . 50) (res-str . $
```


</td>
</tr>
</table>




---


### global-env-size

Get the size (in number of bindings) of the global env. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(global-env-size)
```


</td>
<td>

```clj
104u
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


### is-always-gc

The `is-always-gc` predicate is true if LBM is built with the LBM_ALWAYS_GC debug flag. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(is-always-gc)
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
120853
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
120789
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
128000
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
100000u
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
800000u
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
13803u
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
771u
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
4609u
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
95391u
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
95391u
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
95391u
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
0u
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
2348u
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
0u
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
2348u
```


</td>
</tr>
</table>




---

## Threads


### mailbox-get

`mailbox-get` returns the mailbox contents of a thread as a list. The form of a `mailbox-get` expression is `(mailbox-get pid)`. Note that `mailbox-get` does **NOT** empty the mailbox. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define f (lambda nil
            (progn 
                (sleep 1)
                (f))))
```


</td>
<td>

```clj
(closure nil 
  (progn 
      (sleep 1)
      (f))
  nil)
```


</td>
</tr>
<tr>
<td>

```clj
(define pid (spawn f))
```


</td>
<td>

```clj
14235
```


</td>
</tr>
<tr>
<td>

```clj
(send pid "hello world")
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
(send pid (list 1 2 3))
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
(send pid 'apa)
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
(mailbox-get pid)
```


</td>
<td>

```clj
("hello world" (1 2 3) apa)
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
(0 36 0)
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

This document was generated by LispBM version 0.36.0 

