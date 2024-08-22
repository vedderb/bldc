# List and classification of BUGS found in LBM

## Background

Up until not LBM has been tested using a set of unit-tests.
1. ~550 (and growing) small lisp programs
2. Each program is run on 28 different configurations of the runtime system for a total > 15000 tests
3. All bugs trigger by Robert's methodologies are missed by the unit tests.

## Addition of bytes yields unsigned

```
# (+ 5b 1b)
> 6u
```

## Subtraction of bytes yields type-error

```
# (- 5b 1b)
```

## Irellevant error messages

```
# (mod 4 0)
***   Error: division_by_zero
***   In:    bufset-i32
***   After: 0


> division_by_zero
```

FIXED: commit 79c6918108012adfc7f6693c8ed3e48fceaa49d0

------------------------------------------------------------

# Bug LOG (Thoughts, Roberts input desired)

** INFO TO LOG**
 1. Failing property
 2. Description of property
 3. Counter example
 4. Part of LBM that fails, Parse, eval, flatvalues.. etc
 5. Fix description
 6. git-hash to version of LBM that triggered bug
 7. git-hash of commit to fix bug

** Guidelines

* Never remove a property that found a bug.
* If it turns out a property has a bug
  - If it found no LBM bug: remove it
  - IF it did find an LBM in spite of its own bug: keep it until sure a bugfixed property finds the bug too.
* Properties that never found a bug so far can be updated silently
  - Those that found bugs should be copied and the updated one have some versioning postfix to its name.
* Bugs are added to bug log.
  - Joel can fill in point 4 and 5 for bugs found when Joel is unavailable

# Bug LOG

------------------------------------------------------------

# Bug 1 (feb 20 2024) :heavy_check_mark:

1. prop_env_lbm
2. load an environment into LBM and store it back to a file. Input and output env should contain the same bindings.
3.
4. LBM's flatten_value (serialiser) fails to flatten certain values:
```
 ALERT: A value in the environment was not flattenable
Flat value buffer too small!         
Fatal error during flattening
Value: ((lambda (p3 p2 p1) (set (quote p2) (let ((local-var4 nil) (local-var5 64606899u) (local-var6 3i32) (local-var7 p3)) ((lambda (p8) 1b) p1)))))
ALERT: A value in the environment was not flattenable
Flat value buffer too small!
Fatal error during flattening
Value: ((lambda (p5 p4) (env-fun1 1i64 1b -1i64)))
```
5.
``` 
  case LBM_TYPE_BYTE:
-   return 1;
+   return 1 + 1;
```
6. Trigger: commit 5fc5a7df1b3353ffa11aef4e4a0c7ae8cd6690b1
7. Fix: commit baa5bfbad6ef9a246ed3f4f8e4027638817ffc83

------------------------------------------------------------

# Bug 2 (feb 20 2024)

-- This is a concurrency bug in the LBM REPL environment reader.
-- Better synchronization against the LBM runtime system may be needed
-- while created the loaded back environment.

1. prop_env_lbm
2. load an environment into LBM and store it back to a file. Input and output env should contain the same bindings.
3.
4. Failure to create a symbol upon reading in stored environment.
5.
6. Trigger: commit baa5bfbad6ef9a246ed3f4f8e4027638817ffc83
7. Fix:

------------------------------------------------------------

# Bug 3 (feb 20 2024) :heavy_check_mark:

1. prop_env_lbm
2. load an environment into LBM and store it back to a file. Input and output env should contain the same bindings.
3. Env containing: (ILit TInt64 3298071235)
4. LBM deserializes int64 values incorrectly if they are larger than 2^32.
5.
```
     if (extract_dword(v, &tmp)) {
-      lbm_value im = lbm_enc_i64((int32_t)tmp);
+      lbm_value im = lbm_enc_i64((int64_t)tmp);
```
6. Trigger: commit baa5bfbad6ef9a246ed3f4f8e4027638817ffc83
7. Fix: commit b24e4c3974e24c7bf4c95c716c475d6870300d0d

------------------------------------------------------------

# Bug 4 (feb 21 2024) :heavy_check_mark:

1. prop_add
2. Check if addition is correct when it comes to value and type-promotion.
3. (+ 0b 0b)
4. Return value has been incorrectly promoted to an unsigned
5.
```
   lbm_uint t = (lbm_type_of_functional(a) < lbm_type_of_functional(b)) ? lbm_type_of_functional(b) : lbm_type_of_functional(a);
   switch (t) {
+  case LBM_TYPE_BYTE: retval = lbm_enc_char(lbm_dec_as_char(a) + lbm_dec_as_char(b)); break;
```

```
 static lbm_value fundamental_add(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
   (void) ctx;
-  lbm_uint sum = lbm_enc_u(0);
+  lbm_uint sum = lbm_enc_char(0);
```
6. Trigger: commit 64219f1bd1676d1018057b0180325622d8c57e44
7. Fix: commit de5c356b9c5c4c4593910b46cf7ef0deb17a7da8

------------------------------------------------------------

# Bug 5 (feb 21 2024) 

1. found manually
2. n/a
3. (+ 5u28 1.0f32)
4. Expression is treated as (+ 5u 28 1.0f32), whereas we might have expected a parse error of some kind. It is a bit unclear
5. n/a
6. Trigger: commit 64219f1bd1676d1018057b0180325622d8c57e44
7. Fix: n/a

------------------------------------------------------------

# Bug 6 (feb 21 2024) :heavy_check_mark:

1. prop_add
2. check if addition is correct regarding semantic and type promotion
3. (- 0b)
4. a type-error is thrown
5. n/a
6. Trigger: commit de5c356b9c5c4c4593910b46cf7ef0deb17a7da8
7. Fix: commit fba7e9a97341273faacf917fc33f43fd117ac085 

------------------------------------------------------------

# Bug 7 (feb 21 2024) :heavy_check_mark:

1. prop_add
2. - || -
3. (+ 0u32 env-var23) -- in the environment we have (define env-var23 0)
4. compileAndRun of env-var23 returns a Cons with the value of env-var23, and nil. There should not be a nil
5. n/a
6. Trigger: commit de5c356b9c5c4c4593910b46cf7ef0deb17a7da8
7. Fix: This bug was a result of how the generator populated the environment. The bug existed not in the LBM side, but on this side. It was nonetheless a very nice minimal example.

This bug was discovered with a really nice minimal counter-example

```
===== Env =====
(define env-var23 0)
===== Exps =====
0u32
env-var23
```

------------------------------------------------------------

# Bug 8 (feb 21 2024) 

Incorrect: 0i64 + 0u64 should evaluate to 0u64

1. prop_add
2. - || -
3. (+ 0i64 0u64)
4. this evaluates to 0u64, but the type should be promoted to `u64`
5. n/a
6. Trigger: commit de5c356b9c5c4c4593910b46cf7ef0deb17a7da8
7. Fix: n/a

------------------------------------------------------------

# Bug 9 (feb 22 2024)

1. prop_add
2. - || -
3. (+ -1i32 0u64)
4. This evaluates to 4294967295u64, but should be 18446744073709551615u64
5. n/a
6. Trigger: commit 4a7ac41f0a270fef9ae6aa44b4a3b11f92be291e
7. Fix: commit 520fde1a9610ec05300d72a898dd477553c29624

------------------------------------------------------------

# Bug 10 (feb 22 2024) :heavy_check_mark:

1. prop_add
2. - || -
3. (+ 128b 0.0f64)
4. Should evaluate to 128.0f64, but becomes -128.0f64
5. Pretty substantial changes to handling of the BYTE type.
6. Trigger: commit 4a7ac41f0a270fef9ae6aa44b4a3b11f92be291e
7. Fix: commit 1ea55e8a32bba217a479ff5247c78b5859acd0ca

------------------------------------------------------------

# Bug 11 (feb 22 2024) :heavy_check_mark:

1. prop_add
2. - || -
3. (+ 0u64 (- 1b))
4. Evaluates to 18446744073709551615u64, but should be 255?
5. Fixing Bug 10 also resolved this: shared code envolves decoding of Byte values.
6. Trigger: commit 4a7ac41f0a270fef9ae6aa44b4a3b11f92be291e
7. Fix: commit 1ea55e8a32bba217a479ff5247c78b5859acd0ca

------------------------------------------------------------

# Bug 12 (feb 22 2024) :heavy_check_mark:

1. prop_add
2. - || -
3. (- 0b 0b)
4. Throws a type-error, but is well-formed and well-typed
5. n/a
6. Trigger: commit 4a7ac41f0a270fef9ae6aa44b4a3b11f92be291e
7. Fix: commit fba7e9a97341273faacf917fc33f43fd117ac085

------------------------------------------------------------

# Bug 13 (feb 23 2024)

1. prop_add
2. - || -
3. (+ -10147391 -124070338)
4. Expected result is -134217729, but returns 134217727
5. n/a
6. Trigger: commit 4a7ac41f0a270fef9ae6aa44b4a3b11f92be291e
Fix: bug was on Haskell side, in how we modeled 28bit numbers. Here is the sexp-gen seed to fix: 7de2ab0ad1813d1450c759884b36e35dd7362557

------------------------------------------------------------

# Bug 14 (feb 27 2024)

1. prop_add
2. - || -
3.

environment
```lisp
(define env-fun20 (lambda (p21 p20) p20))

(define env-fun32 (lambda (p33 p32) (and (env-fun20 0b p32) (let ((local-var34 p32)) t))))
```

expression
```lisp
(env-fun32 0i64 0u32)
```

4. It is expected to work, and to compute `0b` (I think), but it crashes saying that `p32` is not bound. If I remove either `e1` or `e2` in `(and e1 e2)`, it works.
   it computes t, which it should as both 0b and t are true. 
5. Store and restore the environment between evaluating each of ANDed values.
   A function call there destroys the environment.
6. Trigger: edb024b75fe37c8f948fccb4e7d992cf783fd429
7. Fix: commit a4957659c287a9b0fab5bf9f1f561fb3a28dfe09

------------------------------------------------------------

# Bug 15 (feb 28 2024)

 1. prop_add
 2. - | | - 
 3.
 ```
 ===== Env =====
(define env-var0 -0.4444444444444444f32)

(define env-fun2 (lambda (p2) (let ((local-var3 (+ -6318676116233991596i64 7789632779833347874i64 131279922u -8242741438836604672i64 -1380498031i32))) (set 'local-var3 (let ((local-var4 17031036726366699265u64) (local-var5 4928743844744272224i64)) local-var5)))))

(define env-fun3 (lambda (p4 p3) 'p4))

(define env-var4 t)

(define env-var6 0.33034422996552093f32)

(define env-fun7 (lambda (p9 p8 p7) (if nil 2123952966837108257i64 -7094765193516988647i64)))

(define env-fun8 (lambda (p8) (<= (if p8 1673493574u32 3655694827u32) (let ((local-var10 'env-var4) (local-var11 p8)) -18908974) (- 1044305357i32 -5328733 1338560558u32 2236767803u32))))

(define env-fun9 (lambda (p12 p11 p10 p9) 9031673))

(define env-fun11 (lambda (p14 p13 p12 p11) (let ((local-var15 (let ((local-var17 (env-fun9 t env-var0 0u 0u64))) (env-fun7 local-var17 0.0f64 'env-fun9))) (local-var16 (let ((local-var17 p12)) 10106852195696325485u64))) (let ((local-var19 ((lambda (p25 p24 p23 p22) 0) nil 0u 'p11 'env-fun8))) (let ((local-var22 p14)) ((lambda (p27) local-var22) p13))))))

(define env-var15 0)
===== Exps =====
---(let ((local-var25 env-var15) (local-var26 t) (local-var27 nil) (local-var28 2279424550u32)) (env-fun9 local-var27 env-var6 208318912u 10854778705472356986u64))
---(let ((local-var25 (env-fun2 'env-fun3))) (set 'local-var25 (- -8346607621290689766i64 -8843221953050836252i64 3585769282u32 2652407276u32)))
=====
expected environment:
Sym "env-fun3" : (closure (p4 p3) 'p4 nil)
Sym "env-fun7" : (closure (p9 p8 p7) (if nil 2123952966837108257i64 -7094765193516988647i64) nil)
Sym "env-fun8" : (closure (p8) (<= (if p8 1673493574u32 3655694827u32) (let ((local-var10 'env-var4) (local-var11 p8)) -18908974) (- 1044305357i32 -5328733 1338560558u32 2236767803u32)) nil)
Sym "env-fun9" : (closure (p12 p11 p10 p9) 9031673 nil)
Sym "env-var0" : -0.4444444477558136f32
Sym "env-var15" : 0
Sym "env-var4" : t
Sym "env-var6" : 0.33034422993659973f32
Sym "local-var22" : (closure (p2) (let ((local-var3 (+ -6318676116233991596i64 7789632779833347874i64 131279922u -8242741438836604672i64 -1380498031i32))) (set 'local-var3 (let ((local-var4 17031036726366699265u64) (local-var5 4928743844744272224i64)) local-var5))) nil)

actual environment:
Sym "env-fun11" : (closure (p14 p13 p12 p11) (let ((local-var15 (let ((local-var17 (env-fun9 t env-var0 0u 0u64))) (env-fun7 local-var17 0.0f64 'env-fun9))) (local-var16 (let ((local-var17 p12)) 10106852195696325485u64))) (let ((local-var19 ((lambda (p25 p24 p23 p22) 0) nil 0u 'p11 'env-fun8))) (let ((local-var22 p14)) ((lambda (p27) local-var22) p13)))) nil)
Sym "env-fun2" : (closure (p2) (let ((local-var3 (+ -6318676116233991596i64 7789632779833347874i64 131279922u -8242741438836604672i64 -1380498031i32))) (set 'local-var3 (let ((local-var4 17031036726366699265u64) (local-var5 4928743844744272224i64)) local-var5))) nil)
Sym "env-fun3" : (closure (p4 p3) 'p4 nil)
Sym "env-fun7" : (closure (p9 p8 p7) (if nil 2123952966837108257i64 -7094765193516988647i64) nil)
Sym "env-fun8" : (closure (p8) (<= (if p8 1673493574u32 3655694827u32) (let ((local-var10 'env-var4) (local-var11 p8)) -18908974) (- 1044305357i32 -5328733 1338560558u32 2236767803u32)) nil)
Sym "env-fun9" : (closure (p12 p11 p10 p9) 9031673 nil)
Sym "env-var0" : -0.4444444477558136f32
Sym "env-var15" : 0
Sym "env-var4" : t
Sym "env-var6" : 0.33034422993659973f32

=====
Individually evaluated expressions:
e1 : (let ((local-var25 env-var15) (local-var26 t) (local-var27 nil) (local-var28 2279424550u32)) (env-fun9 local-var27 env-var6 208318912u 10854778705472356986u64))
e2 : (let ((local-var25 (env-fun2 'env-fun3))) (set 'local-var25 (- -8346607621290689766i64 -8843221953050836252i64 3585769282u32 2652407276u32)))
Should together produce the same result as:
(add e1 e2) = (+ (let ((local-var25 env-var15) (local-var26 t) (local-var27 nil) (local-var28 2279424550u32)) (env-fun9 local-var27 env-var6 208318912u 10854778705472356986u64)) (let ((local-var25 (env-fun2 'env-fun3))) (set 'local-var25 (- -8346607621290689766i64 -8843221953050836252i64 3585769282u32 2652407276u32))))
=====
expected result : ILit TInt64 496614325531001601
actual result   : ILit TInt64 496614325531001601
=====
``` 

 4. This is most likely a synchronisation issue between the LBM runtime system
    and the REPL process. The REPL sends messages to LBM when it loads in an
    environment file for each binding. 
 5. Improve synchronisation by waiting until the key,value binding can be observed
    from the outside to be present in the environment.
 6. Trigger: commit a4957659c287a9b0fab5bf9f1f561fb3a28dfe09
 7. Fix: commit 2f47403e913876df57f2eeedb294b9532ce1d581

------------------------------------------------------------

# Potential Bug 16 (Feb 29 2024)

1. prop_eq
2. assert equals to be correct
3.

```lisp
(define env-var64 0u32)

(eq 0i32 t (setq env-var64 1u32))
```

The above program terminates with the final value of `env-var64` being `1u32`, whereas I expected it to short-circuit
as e.g. `and` and `or`. It should compare the first two elements and see that they are not equal, and then never evaluate
the third. Might not be a bug, but it raising an interesting question of semantics. Joel will think about what
should be the correct semantics.

QuickCheck seeds on Roberts machine
```haskell
seed :: QCGen
seed = read "SMGen 2402507914284866448 17817352536072034693"

size = 20
```

4. n/a
5. n/a
6. Trigger: commit dea3279fd7656df2030084f5e33df5653761d613
7. n/a

------------------------------------------------------------

# Potential Bug 17 (Feb 29 2024)

1. prop_num_eq
2. assert equals to be correct on numbers
3.

```lisp
(= 0b 1b)
```

The above program returns `t`, but the answer should clearly be `nil`.

QuickCheck seeds on Roberts machine
```haskell
seed :: QCGen
seed = read "SMGen 4646947586283255503 17545856198110524507"

size = 5
```

4. LBMs compare operation was missing a case for Byte typed comparison.
5. Add a case in compare for byte typed values.
6. Trigger: commit dea3279fd7656df2030084f5e33df5653761d613
7. Fix: commit fbc2d92b7636fa044efd95c94df8f4612505518b

------------------------------------------------------------