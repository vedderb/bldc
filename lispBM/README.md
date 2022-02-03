# LispBM



This is the VESC-integration of [lispBM](https://github.com/svenssonjoel/lispBM) written by Joel Svensson. It allows the VESC to run lisp-programs in a sandboxed environment.

### Feature Overview

* Development and testing in VESC Tool with variable live monitoring and plotting as well as CPU and memory monitoring.
* Sandboxed environment, meaning that the Lisp code (hopefully) cannot freeze or crash the rest of the VESC code when it gets stuck or runs out of heap or stack memory.
* The application runs on the VESC itself without the need for having VESC Tool connected and is stored in flash memory.
* When a lisp-application is written to the VESC it is automatically started on each boot.


## Documentation

Basics about LispBM are documented [here](http://svenssonjoel.github.io/lbmdoc/html/lbmref.html). The VESC-specific extensions are documented in this section. Note that VESC Tool includes a collection of examples that can be used as a starting point for lisp usage on the VESC.

### Various commands

#### print

```clj
(print arg1 ... argN)
```

Print things in the VESC Tool Lisp console. Example:

```clj
(print "Hello World")
```

Should work for all lisp types.

#### timeout-reset

```clj
(timeout-reset)
```

Reset the timeout that stops the motor. This has to be run on a regular basis to keep the motor running.

#### get-ppm

```clj
(get-ppm)
```

Read the decoded value on the PPM input and returns 0 to 1. Note that the PPM app has to be configured and running. Example:

```clj
(print (list "PPM Value: " (get-ppm)))
```

### Motor set commands

#### set-current

```clj
(set-current current)
```

Set motor current in amperes.

#### set-current-rel

```clj
(set-current-rel current)
```

Set motor current relative to the maximum current. Range -1 to 1. For example, if the maximum current is set to 50A, (set-current-rel 0.5) will set the current to 25A.

#### set-duty
```clj
(set-duty dutycycle)
```

Set duty cycle.

## How to update

To update from remote repository:

```
git remote add lispBM git@github.com:svenssonjoel/lispBM.git
git subtree pull --squash --prefix=lispBM/lispBM/ lispBM master
```

The first command might fail if it already is added, but the second one should still work. If there are uncomitted changes you can run **git stash** before the commands and **git stash pop** after them.