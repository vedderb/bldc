# LBM Gotchas and Caveats

This document collects caveats and gotchas as they are discovered. Hopefully these will
also be given efficient solutions at some points in which case the solution to that gotcha
will also be documented here.

If you find any more gotchas, please let me know how to trigger them! Would be much appreciated.

## Environment

In LBM there is a global environment which is an association list of `(symbol . value)` pairs.
There is also a local environment that is carried along in the evaluator while evaluating
expressions. This local environment is also an association list `(symbol . value)` pairs.

### Closure Gotcha!

When a closure is created, a reference to the local environment is stored in the closure object.
This is for efficiency reasons as traversing the expression and pulling in all free variables
would be a somewhat costly operation (order of the size of the expression) while saving a reference
to the local environment is an O(1) operation. This essentially trades some space (as it potentially
prohibits GC from removing some dead values during the life-time of that closure) for improved performance.

Note that the global environment is not "saved" in the same way inside the closure. Given that
`undefine` exists in LBM, this is a potential foot-gun as illustrated by the example below:

```clj
# (define a 10)
> 10
# (define f (lambda (x) (+ x a)))
> (closure (x) (+ x a) nil)
# (undefine 'a)
> ((f closure (x) (+ x a) nil))
# (f 1)
***	Error:	variable_not_bound
***		a
# 
***	Between rows: (-1 unknown) 
***		Start: -1
***		End:   -1
# 
> variable_not_bound
#
```

Currently no (efficient) solution in mind for this gotcha. Just be careful if you use `undefine`.


## Flash memory




