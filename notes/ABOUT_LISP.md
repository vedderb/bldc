

# About Lisp

* Developed by John McCarthy in 1958.
* Paper "Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I" in 1960.
* Many dialects and variants since then (list from wikipedia): 
  * Lisp 1, 1.5 and 2.
  * MACLISP
  * InterLisp
  * Lisp Machine Lisp
  * Scheme
  * Nil
  * Common Lisp							
  * Le Lisp
  * T
  * Emacs Lisp		(Configuration and customization language for Emacs)		
  * AutoLisp
  * OpenLisp
  * PicoLisp
  * EuLisp
  * ISLISP
  * newLISP
  * Racket		(A language implementation toolbox)
  * GNU Guile
  * Visual LISP
  * Clojure		(running on JVM)		
  * Arc
  * LFE
  * Hy			(www.github.com/hylang/hy)
  * A huge amount of hobbyist variants.
* In the beginning, popular in the AI community. 


## Sources for further information about lisp

* Structure and Interpretation of Computer Programs.
* YouTube lectures by Hal Abelson and Gerald Jay Sussman (These are awesome).
* Build your own lisp (book online free by Daniel Holden, www.buildyourownlisp.com).
 
 
## What I think of when hearing LISP

* Lots of parenthesis. 
* Dynamically typed.
* Functional.
  * Higher order functions.
  * Lambdas, Closures. 
* Homoiconicity
* Garbage collection. Automatic memory management.
* cons, car, cdr!
* Small when it comes to "syntax".
* FUN!


## About this attempt at a LISP

* Motivations in order of importance!
  1. FUN!
  2. Learning new stuff.
  3. An interactive REPL for the ZYNQ devboard.
  4. Never ending supply of fun programming excercises.


* My wish list (partially implemented)
  1. Cons cell based memory with garbage collection.
  2. Lambdas and closures.
  3. Arrays.
  4. Some low level features (direct access memory mapped registers).
  5. Capability to run infinitely looping functions: (define f (lambda () (f)))
  6. Capability to load code and data from SD card (If compiled for Zynq).
  7. Compilation to a home-made byte code (JIT-ish, when using repl or eval for example. But also off line).
  