# LispBM Pattern Extensions (Lua match) Reference Manual

The pattern extensions provide Lua-style pattern matching on strings and byte-arrays: `str-lua-find`, `str-lua-match` and `str-lua-gmatch`. 

The matching engine itself is a port of the pattern-matching core of Lua 5.5's `lstrlib.c`, adapted to run standalone (without a `lua_State`) and with LispBM's error-signaling conventions instead of Lua's. 

## Pattern Syntax


---

`str-lua-find`, `str-lua-match` and `str-lua-gmatch` all share the same Lua-style pattern language. It is not a full regular expression language, but it covers the common cases and stays small enough to run predictably on embedded targets. 

**Character classes**, each of these matches a single character. 

|Class || 
 |----|----|
 `.` | Any character.
 `%a` / `%A` | A letter / not a letter.
 `%c` / `%C` | A control character / not a control character.
 `%d` / `%D` | A digit / not a digit.
 `%g` / `%G` | A printable character other than space / not one.
 `%l` / `%L` | A lowercase letter / not a lowercase letter.
 `%p` / `%P` | A punctuation character / not a punctuation character.
 `%s` / `%S` | A space character / not a space character.
 `%u` / `%U` | An uppercase letter / not an uppercase letter.
 `%w` / `%W` | An alphanumeric character / not an alphanumeric character.
 `%x` / `%X` | A hexadecimal digit / not a hexadecimal digit.
 `%c` where `c` is not a letter | The literal character `c`. This is how the magic characters `( ) . % + - * ? [ ] ^ $` are escaped, e.g. `%%`, `%.`, `%(`.
 

**Sets, quantifiers and anchors** 

|Item || 
 |----|----|
 `[set]` | Any single character in `set`, e.g. `[%w_]` or `[a-z]`. Classes from the table above may appear inside a set.
 `[^set]` | Any single character not in `set`.
 `*` | Zero or more repetitions of the previous item, as many as possible.
 `+` | One or more repetitions of the previous item, as many as possible.
 `-` | Zero or more repetitions of the previous item, as few as possible.
 `?` | Zero or one repetition of the previous item.
 `^` | Anchors the match to the start of `text`. Only special as the first character of a pattern.
 `$` | Anchors the match to the end of `text`. Only special as the last character of a pattern.
 

A set may include ranges written `x-y`, meaning any character from `x` to `y` inclusive, such as `[a-z]` or `[0-9]`. Ranges can be mixed freely with single characters and classes in the same set, e.g. `[a-zA-Z0-9_]`. A `-` that is not between two characters, such as at the very end of a set, is treated as a literal hyphen instead of a range. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-lua-match "Hello World" "[a-z]+")
```


</td>
<td>

```clj
"ello"
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-match "Hello World" "[A-Z][a-z]*")
```


</td>
<td>

```clj
"Hello"
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-match "abc123def" "[^0-9]+")
```


</td>
<td>

```clj
"abc"
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-match "foo-bar_baz" "[a-z-]+")
```


</td>
<td>

```clj
"foo-bar"
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-find "port: 8080" "[0-9]+")
```


</td>
<td>

```clj
(6 10 "8080")
```


</td>
</tr>
</table>

**Captures and other special items** 

|Item || 
 |----|----|
 `(pattern)` | Captures the text matched by `pattern`.
 `()` | A position capture: captures the current 0-based offset into `text` instead of any text.
 `%1` .. `%9` | Back-reference: matches exactly the text captured by capture number `1`-`9` earlier in the same pattern.
 `%bxy` | Balanced match: matches from the first `x` to the matching `y`, where `x` and `y` are any two distinct characters, e.g. `%b()` or `%b[]`.
 `%f[set]` | Frontier pattern: matches the empty string at a transition from a character not in `set` to a character in `set`.
 

Two details differ from stock Lua, both mentioned above as well: byte offsets (from `str-lua-find` and from position captures) are 0-based rather than 1-based, and `string.gsub` is not part of this API; only searching (`str-lua-find`), extracting (`str-lua-match`) and iterating (`str-lua-gmatch`) are provided. 

## Functions


### str-lua-find

`str-lua-find` searches `text` for the first match of a Lua-style pattern and reports where the match was found. The form of a `str-lua-find` expression is `(str-lua-find text pattern opt-start-offset opt-max-iterations)`. 

|Arg || 
 |----|----|
 `text` | The string (or byte-array) to search in.
 `pattern` | A Lua-style pattern, see the Pattern Syntax section.
 `opt-start-offset` | Byte offset to start searching from. Defaults to `0`.
 `opt-max-iterations` | Caps the backtracking work spent on this call. Defaults to `100000`, see Errors and Safety Limits.
 

On success `str-lua-find` returns a list `(start end result)`. `start` and `end` are 0-based byte offsets delimiting the match (`end` is exclusive). `result` is the whole matched substring if the pattern has no capture groups, or a list of the captured values, left to right, if it does. `nil` is returned when there is no match. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-lua-find "hello world" "wor")
```


</td>
<td>

```clj
(6 9 "wor")
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-find "hello world" "xyz")
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
(str-lua-find "key=value" "(%a+)=(%a+)")
```


</td>
<td>

```clj
(0 9 ("key" "value"))
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-find "hello world" "o" 5)
```


</td>
<td>

```clj
(7 8 "o")
```


</td>
</tr>
</table>

`str-lua-find` also works directly on byte-arrays, searching up to the first zero byte or the end of the array, the same convention used throughout the rest of the string library. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define bin (bufcreate 5))
```


</td>
<td>

```clj
[0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(bufset-u8 bin 0 65)
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
(bufset-u8 bin 1 66)
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
(bufset-u8 bin 2 0)
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
(bufset-u8 bin 3 67)
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
(bufset-u8 bin 4 68)
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
(str-lua-find bin "B")
```


</td>
<td>

```clj
(1 2 "B")
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-find bin "C")
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


### str-lua-match

`str-lua-match` searches `text` for the first match of a Lua-style pattern and returns the matched data itself, rather than its position. The form of a `str-lua-match` expression is `(str-lua-match text pattern opt-start-offset opt-max-iterations)`, with the same arguments as `str-lua-find`. 

If the pattern has no capture groups, the whole matched substring is returned. If the pattern has one or more capture groups, a list of the captured values is returned instead, left to right, one entry per capture group, even when there is only a single capture. This is a slight departure from Lua itself, where `string.match` returns a single capture as a bare value using multiple return values, something LispBM does not have. `nil` is returned when there is no match. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-lua-match "abc123" "%a+")
```


</td>
<td>

```clj
"abc"
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-match "xabc" "^abc$")
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
(str-lua-match "key=value" "(%a+)=(%a+)")
```


</td>
<td>

```clj
("key" "value")
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-match "abcabc" "(abc)%1")
```


</td>
<td>

```clj
("abc")
```


</td>
</tr>
</table>

A capture written as an empty pair of parentheses, `()`, is a *position capture*: instead of capturing text it captures the 0-based offset into `text` at that point in the match. This, too, differs from stock Lua, which numbers positions starting at 1. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-lua-match "hello" "()ll()")
```


</td>
<td>

```clj
(2 4)
```


</td>
</tr>
</table>

Two of the more unusual pieces of pattern syntax, balanced-match and frontier patterns, are supported as well. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-lua-match "(foo (bar) baz)" "%b()")
```


</td>
<td>

```clj
"(foo (bar) baz)"
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-match "THE (quick) fox" "%f[%a]%u+%f[%A]")
```


</td>
<td>

```clj
"THE"
```


</td>
</tr>
</table>

A malformed pattern is reported as an evaluation error rather than returning `nil`, and must be caught with `trap` if the pattern is not known to be well-formed ahead of time (see Errors and Safety Limits below). 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(trap (str-lua-match "abc" "abc%"))
```


</td>
<td>

```clj
(exit-error eval_error)
```


</td>
</tr>
</table>




---


### str-lua-gmatch

`str-lua-gmatch` finds successive, non-overlapping matches of a pattern in `text`, one match per call. The form of a `str-lua-gmatch` expression is `(str-lua-gmatch text pattern state opt-max-iterations)`. 

Unlike Lua's `string.gmatch`, which hands back a stateful iterator closure, `str-lua-gmatch` is a plain function that threads its search position through an explicit `state` argument. Pass `nil` as `state` to find the first match. Each call returns either `nil`, when there are no more matches, or a pair `(result . next-state)`, where `result` has the same shape as a `str-lua-match` result (a string, or a list of captures) and `next-state` is an integer to pass back in as `state` on the following call. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-lua-gmatch "one two three" "%a+" nil)
```


</td>
<td>

```clj
("one" . 3)
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-gmatch "one two three" "%a+" 3)
```


</td>
<td>

```clj
("two" . 7)
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-gmatch "one two three" "%a+" 7)
```


</td>
<td>

```clj
("three" . 13)
```


</td>
</tr>
<tr>
<td>

```clj
(str-lua-gmatch "one two three" "%a+" 13)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>

The intended way to use `str-lua-gmatch` is a loop that keeps calling it with the previous `next-state`, until it returns `nil`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(defunret collect-all (text pattern) (progn 
                                         (var acc nil)
                                         (var state nil)
                                         (loopwhile t
                                                    (progn 
                                                        (var r (str-lua-gmatch text pattern state))
                                                        (if (eq r nil) (return (reverse acc)) (progn 
                                                                                                  (setq acc (cons (car r) acc))
                                                                                                  (setq state (cdr r))))))))
```


</td>
<td>

```clj
(closure (text pattern) 
  (call-cc-unsafe (lambda (return)
                    (progn 
                        (var acc nil)
                        (var state nil)
                        (loopwhile t
                                   (progn 
                                       (var r (str-lua-gmatch text pattern state))
                                       (if (eq r nil) (return (reverse acc)) (progn 
                                                                                 (setq acc (cons (car r) acc))
                                                                                 (setq state (cdr r)))))))))
  nil)
```


</td>
</tr>
<tr>
<td>

```clj
(collect-all "one two three" "%a+")
```


</td>
<td>

```clj
("one" "two" "three")
```


</td>
</tr>
</table>

Patterns that can match the empty string, such as `a*`, are handled the same way Lua handles them: an empty match is not allowed to repeat at the exact position of the previous match, so `str-lua-gmatch` automatically steps one byte forward instead of looping forever. 




---

## Errors and Safety Limits


---

A syntactically invalid pattern is reported as an evaluation error rather than as a `nil` result, and needs to be caught with `trap` if it is not known to be well-formed ahead of time. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(trap (str-lua-match "abc" "abc%"))
```


</td>
<td>

```clj
(exit-error eval_error)
```


</td>
</tr>
<tr>
<td>

```clj
(trap (str-lua-match "abc" "[abc"))
```


</td>
<td>

```clj
(exit-error eval_error)
```


</td>
</tr>
<tr>
<td>

```clj
(trap (str-lua-match "abc" "%b("))
```


</td>
<td>

```clj
(exit-error eval_error)
```


</td>
</tr>
<tr>
<td>

```clj
(trap (str-lua-match "abc" "%f"))
```


</td>
<td>

```clj
(exit-error eval_error)
```


</td>
</tr>
</table>

The reason for the failure is attached to the error and, if uncaught, printed by the REPL. The possible reasons are: 

   - pattern ends with '%'
   - missing ']' in pattern
   - missing arguments to '%b'
   - missing '[' after '%f'
   - too many captures in pattern
   - invalid pattern capture (unmatched ')')
   - invalid or unfinished capture index
   - pattern nested too deeply
   - pattern matching exceeded the iteration limit

The last two reasons come from two independent safety limits, both of which exist because a pattern-matching call runs to completion as a single, non-preemptible C call and would otherwise be able to stall the whole runtime rather than just one thread. 

The first limit bounds how deeply patterns may nest, catching patterns with, for example, hundreds of nested capture groups. The second limit, `opt-max-iterations` (the last argument to `str-lua-find`, `str-lua-match` and `str-lua-gmatch`, defaulting to 100000), bounds the total amount of backtracking work a single call may perform, catching patterns such as chained `.*` groups against a long, non-matching string that would otherwise take exponential time while barely growing the call stack at all. 

This document was generated by LispBM version 0.37.0 

