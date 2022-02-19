
# Random thoughts about adding pattern matching to lispBM 


I want to add a match and a recv primitive. The `match` should implement pattern matching as a programming abstraction. 
The `recv` implements reception of messages for concurrency purposes. 

There are so many choices though... I dont know where to start! 



## Syntactic look and feel

**Taking a list of pattern->exp mappings**
``` 
(recv ((pattern1 . what-to-do1)
       (pattern2 . what-to-do2)
       (...)
       (patternN . what-to-doN)))
```

```
(match e ((pattern1 . what-to-do1)
          (pattern2 . what-to-do2)
          (...)
          (patternN . what-to-doN)))
```



**taking an arbitrary number of arguments that each are pattern->exp mappings**
```
(recv (pattern1 . what-to-do1)
      (pattern2 . what-to-do2)
      (...)
      (patternN . what-to-doN))
```
This approach matches up with how `progn` in implemented in lispBM.


Even with this simple sketch there are so many lispy options to concider. 
1. should the argument to `recv` (for example) be evaluated? 
   If that argument is evaluated the above example would look like: 
   
   ``` 
   (recv '((pattern1 . what-to-do1)
           (pattern2 . what-to-do2)
           (...)
           (patternN . what-to-doN)))
   ```

	*or* 
	
	``` 
   (recv '(pattern1 . what-to-do1)
         '(pattern2 . what-to-do2)
         '(...)
         '(patternN . what-to-doN))
   ```

	If doing this then you could write `(recv (f x))` where `f` is a
    function that returns a list of pattern->exp mappings. Could that be fun? 
	
	`recv` and `match` will be special forms in lispBM, so any strategy can be 
	picked for how to process the arguments of these in the runtime system. 
2. Should each pattern be evaluated? 
3. Should the exp be evaluated into some final form exp that will in turn be evaluated on a match. 


# What should patterns be made of?

I've been watching the SICP videos and recently the one on pattern matching. That stuff 
seems really general! It makes patters from symbols, (? x), (?c c), and so on and instantiates into skeletons 
using :. The meaning of `: e` really just seems to be "evaluate the arbitrary expression e in the environment 
created while matching the pattern". 

My current plan is to match `pattern` and build an environment, then just simple evaluate `what-to-do` in that 
env. So it is a somewhat simpler form I guess. 

Anyway! what should patterns look like? 

**Currently thinking about something like:** 
1. Symbols match exactly. That is `foo` matches `foo`, `nil` matches `nil` 
2. constants such as 1,2,3,4,5,... matches exacly 1,2,3,4,5,6,... 
3. `_` matches anything
4. (a . b) matches exactly a pair where car matches a and cdr matches b
5. (a1 a2 ... aN) matches exactly a list where a1,a2,...,aN matches the elements of that list. 
6. (? x) matches anything and binds that anything to `x` in the environment we are constructing. 
7. (?<type\_indicator> x) matches anything of type type\_indicator and binds that anything to `x` in the environment.


``` 
(recv ((foo 1 (? x)) . (+ x 10))
      ((bar (? y)) . y)
      (_ . ()))
```
What if in `(?foo x)`, `?foo` is an arbitrary predicate that is evaluated on the part of the expression we match 
against? And the part of the expression is bound to x if that predicate evaluates to true. This feels like a rabbit-hole!


# Guards 

Having case guards feels useful. What could those look like in this context?

in Haskell: 
```
apa = 
  case x of
    Just n | n > 10 -> f n
	_ -> something else
```
