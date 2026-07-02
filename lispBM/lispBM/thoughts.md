
# Thoughts


## Better errors when on platform with lots of memory

 1. Build a  heap-cell -> srcloc table in RAM when reading source.
    look up when error.

 2. Callstack: Saving a callstack is not trivial on a continuation
    passing style evaluator. But it may be possible via some tricks.
    Only do this when on platform with lots of extra RAM to use.
    

    
 
