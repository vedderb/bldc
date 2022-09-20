
# Chapter 3: Concurrency

LispBM (LBM) supports concurrently executing
processes and let's processes communicate with eachother via message
passing. Currently the runtime system that execute LBM programs is
entirely sequential and does not make use of multiple cores.
Concurrency abstractions are very useful even with there not
being any actual parallelism.

In LispBM concurrent processess are managed by a round-robin scheduler that
fairly splits up the usage of the evaluator between the processes.  Each
process as gets a quota of evaluation steps each time it is scheduled and
runs until the quota is used up or the proceses itself chooses to sleep by
yielding. A process can yield explictly using the `yield` command or
implicitly by blocing on for example a message passing receive
operation, `recv`.


## Getting started with concurrent programming in LBM

The `yield` function is used to put a process to sleep for some number
of microseconds. When a process yields it gives up the rest of its quota
for this instance and frees up the runtime system to perform other work.

We can define a `sleep` function that puts a process to sleep for a number of
seconds instead.

```lisp
(defun sleep (x)
  (yield (* x 1000000)))
``` 

You can write a function that prints "hello" ten seconds from now as:

```lisp
(defun hello ()
  (progn
    (sleep 10)
    (print "hello")))
``` 

This program is stored in the file [hello.lisp](ch3_examples/hello.lisp)
and you can load it into the REPL if you launched the REPL from the directory where the file is stored:

```
# :load hello.lisp
filename: hello.lisp
> hello
# 
```

And now we can run the `hello` function by typing `(hello)` and
hitting return:

```
# (hello)
hello
> t
``` 

That it took 10 seconds for the string "hello" to appear is hard to
show off here ;).


We can make the hello program call itself recursively to repeatedly
print hello every 10 seconds:

```lisp
(defun hello ()
  (progn
    (sleep 10)
    (print "hello")
    (hello)))
```

Running this program will look something like:
```
# :load hello_repeater.lisp
filename: hello_repeater.lisp
> hello
# (hello)
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
...
```

While the hello program is running, you can still interact with the REPL.
For example we can squeeze in an evaluation of `(+ 1 2)`:

```
hello
hello
hello
# (+ 1 2)
> 3
hello
hello
``` 

When we start evaluation of some expression, such as `(hello)` or `(+ 1 2)`,
from the REPL an evaluation context for that expression is created
in the runtime system. An evaluation context is a datastructure maintaining
the evaluation stack and other important data related to the evaluation
of a program. So, everything launched from the REPL is in essense "spawned"
as a separate process running concurrently with anything else. 

Processes can also be spawned from within programs using the `spawn` operation.

The following example can be found in [hello_bye.lisp](ch3_examples/hello_bye.lisp).

```lisp
(defun hello ()
  (progn
    (sleep 10)
    (print "hello")
    (hello)))


(defun bye ()
  (progn
    (sleep 5)
    (print "bye")
    (bye)))

(spawn hello)
(bye)
``` 

The example above spawns a hello process that says "hello" every 10 seconds
and then it runs the function `bye` which says "bye" every 5 seconds.

```
# :load hello_bye.lisp 
filename: hello_bye.lisp
bye
hello
bye
bye
hello
bye
bye
hello
bye
bye
hello
bye
``` 

When loading a file into the REPL, everything is evaluated from the
top towards the bottom in an evaluation context. When `(spawn hello)`
is encountered, a new context is created for the evaluation of `hello`
and it is placed on a queue of runnable contexts. After putting the `hello`-context on
the runnable queue, the `bye` function is called. The `bye` function will now
run indefinitely in the original context.

## Context queues

The LBM runtime system maintains queues of evaluation contexts. You can list
the contexts from the repl using the command `:ctxs`.


If the `:ctxs` command is executed while the hello_bye.lisp program is
running you get some feedback as below: 

```
hello
bye
bye
# :ctxs
****** Running contexts ******
--------------------------------
ContextID: 475
Stack SP: 4
Stack SP max: 15
Value: t
--------------------------------
ContextID: 145
Stack SP: 4
Stack SP max: 47
Value: t
****** Blocked contexts ******
****** Done contexts ******
``` 

There are two context in the queue of runnable (running) contexts. This queue
holds all contexts that are waiting to be executed and they end up
on this list when they are started or when they evaluate a `yield` (sleep).

There is a queue of blocked contexts that processes will end up on
if they are blocked waiting for messages (see Message passing).
Finally there is a done queue for contexts that have finished evaluating.
The REPL actually removes contexts from the done queue as soon as it has
printed the final result of that context to the user. 

The context that really, currently, is running at the exact same time as
the `:ctxs` command is issued is not present on any queue and thus not listed
by the `:ctxs` command.

The information printed by the `:ctxs` commands consists of a ContextID which
is assigned to a context upon creation (this value is also the result returned
by `spawn` when first spawning the process). Next is the current stack pointer
of the context and the maximum stack pointer value ever occured. The "Value" refers
to the last value computed by that context. If the context is in the running queue,
the `Value` should be `t` for true, the return value of `yield` when successful. 

## Message passing

All LispBM processes have a mailbox for reception of messages. To send
a message to a process you need to know its ContextID as this is used
as the "address" for that process.

If we spawn a process,for example:

```
# (spawn (lambda (x) (+ x 1)) 1)
> 421
> 2
```

the `spawn` operation will return the context ID, in this case 421. In
the example the process itself computes the value 2.

To send a message the `send` function is used. This function takes a
context ID and a msg as argument. The message can be any LispBM value
(lists, number, a function and so on).

If you send a message to a process, ideally that process should try to
receive messages and this is done using the `recv` form which is very similar
to a `match`. It uses pattern matching to fetch a message from the mailbox
that matches the pattern.

```lisp

(defun f ()
  (progn
    (recv
     ( monkey (print "That's an ape!"))
     ( cat    (print "What a cute cat!"))
     ( _      (print "I dont know what that is!")))
    (f))) 
  
(spawn f)
```

The program above can be found in
[recv_message.lisp](ch3_examples/recv_message.lisp).  The `f` function
loops forever due to the recursive call `(f)` at the end.  The process
blocks at the `recv`, this means that if the mailbox does not contain
any message that fits the patterns, the process goes to sleep until a
new message arrives. While the process is waiting for another message
it will reside in the blocked queue in the runtime system. In this
case, the recv is waiting for a message consisting of the symbol
`monkey` or the symbol `cat` and the last case `_` matches any other
message.

Let's load the program and try to send some messages to the `f` process.

```
# :load recv_message.lisp
filename: recv_message.lisp
> 193
```

This loaded and evaluated the file. The result `193` is the context ID of the
context that is running the `f` function. To send a message to the process:

```
# (send 193 'monkey) 
> t
That's an ape!
```

we can see that send was successful, `t`,  and that the recv form in `f`
correctly chose the "that's an ape!" path.

Trying again with another message:

```
# (send 193 'cat)
> t
What a cute cat!
```

If we send any message that is not a cat or monkey to the process
we should get the last case:

```
# (send 193 1)
> t
I dont know what that is!
```

or

```
# (send 193 (lambda (x) (+ x 1)))
> t
I dont know what that is!
```

---
**NOTE**

Ideally you should bind the result of `spawn` to a name and then use
that name for all interaction with the process.

Example:

```
# (define knower-of-animals (spawn f))
> knower-of-animals
```

```
# (send knower-of-animals 'monkey)
> t
That's an ape!
```
---

***NOTE***

If you send a message to a process and that process has no `recv`, the
message will forever remain in that process' mailbox. Messages that
just sit unused in some mailbox are very wasteful as they cannot be
garbage collected.

The same thing happens if you send messages to a process that HAS a
recv but no case that matches the message format you sent. This can
be helped by always having a catch-all case in your `recv`.
An example of a catch-all is the `( _ (print "I dont know what that is!")))` case
in the example used above. Another catch-all would be the pattern `(? x)`
as in [recv_message2.lisp](ch3_examples/recv_message2.lisp):

```
(defun f ()
  (progn
    (recv
     ( monkey (print "That's an ape!"))
     ( cat    (print "What a cute cat!"))
     ( (? x)  (print "I dont know kind of animal a " x " is!")))
    (f))) 
  
(spawn f)
```

The `(? x)` pattern also matches anything, just like `_` but it binds the
anything to the name x.

``` 
# :load recv_message2.lisp
filename: recv_message2.lisp
> 496
# (send 496 'horse)
> t
I dont know kind of animal a horse is!
```
---

## Example with concurrency and message passing

To appear as soon as I come up with something fun
