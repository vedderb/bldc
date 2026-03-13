# LispBM String Extensions Reference Manual

The string extensions provide functions for manipulating strings, converting between strings and other types, and searching strings. These extensions may or may not be present depending on the platform and configuration of LispBM. 

In LispBM, strings are byte arrays terminated by a null byte. String indices are zero-based and measured in bytes. 

## Conversion


### str-from-n

`str-from-n` converts a number to its string representation. The form is `(str-from-n number)` or `(str-from-n number format)` where `format` is an optional printf-style format string. Floats default to `"%g"` format, integers default to `"%d"`. The result is at most 100 characters long. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-from-n 42)
```


</td>
<td>

```clj
"42"
```


</td>
</tr>
<tr>
<td>

```clj
(str-from-n 3.140000f32)
```


</td>
<td>

```clj
"3.14"
```


</td>
</tr>
<tr>
<td>

```clj
(str-from-n 255 "%x")
```


</td>
<td>

```clj
"ff"
```


</td>
</tr>
<tr>
<td>

```clj
(str-from-n 3.141590f32 "%.2f")
```


</td>
<td>

```clj
"3.14"
```


</td>
</tr>
</table>




---


### str-to-i

`str-to-i` parses a string as an integer. The form is `(str-to-i string)` or `(str-to-i string base)`. The optional `base` argument specifies the number base. Base 0 auto-detects the base from the string prefix (`0x` for hex, `0` for octal, otherwise decimal). Returns an `i32`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-to-i "42")
```


</td>
<td>

```clj
42i32
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-i "-100")
```


</td>
<td>

```clj
-100i32
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-i "ff" 16)
```


</td>
<td>

```clj
255i32
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-i "0xff" 0)
```


</td>
<td>

```clj
255i32
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-i "101" 2)
```


</td>
<td>

```clj
5i32
```


</td>
</tr>
</table>




---


### str-to-f

`str-to-f` parses a string as a floating-point number. The form is `(str-to-f string)`. Returns an `f32`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-to-f "3.14")
```


</td>
<td>

```clj
3.140000f32
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-f "-2.718")
```


</td>
<td>

```clj
-2.718000f32
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-f "1e10")
```


</td>
<td>

```clj
10000000000.000000f32
```


</td>
</tr>
</table>




---


### to-str

`to-str` converts one or more LispBM values to a string. The form is `(to-str val1 val2 ...)`. Multiple values are separated by a single space. The result is at most 300 characters long. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(to-str 42)
```


</td>
<td>

```clj
"42"
```


</td>
</tr>
<tr>
<td>

```clj
(to-str 3.140000f32)
```


</td>
<td>

```clj
"3.140000f32"
```


</td>
</tr>
<tr>
<td>

```clj
(to-str 'hello)
```


</td>
<td>

```clj
"hello"
```


</td>
</tr>
<tr>
<td>

```clj
(to-str 1 2 3)
```


</td>
<td>

```clj
"1 2 3"
```


</td>
</tr>
<tr>
<td>

```clj
(to-str "hello" "world")
```


</td>
<td>

```clj
"hello world"
```


</td>
</tr>
</table>




---


### to-str-delim

`to-str-delim` converts one or more LispBM values to a string using a custom delimiter between values. The form is `(to-str-delim delimiter val1 val2 ...)`. The first argument is the delimiter string. The result is at most 300 characters long. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(to-str-delim ", " 1 2 3)
```


</td>
<td>

```clj
"1, 2, 3"
```


</td>
</tr>
<tr>
<td>

```clj
(to-str-delim " | " 'a 'b 'c)
```


</td>
<td>

```clj
"a | b | c"
```


</td>
</tr>
<tr>
<td>

```clj
(to-str-delim [0] "Hello" " " "World")
```


</td>
<td>

```clj
"Hello World"
```


</td>
</tr>
</table>




---

## String Operations


### str-len

`str-len` returns the length of a string in characters, not counting the null terminator. The form is `(str-len string)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-len "Hello")
```


</td>
<td>

```clj
5
```


</td>
</tr>
<tr>
<td>

```clj
(str-len [0])
```


</td>
<td>

```clj
0
```


</td>
</tr>
<tr>
<td>

```clj
(str-len "Hello, World!")
```


</td>
<td>

```clj
13
```


</td>
</tr>
</table>




---


### str-part

`str-part` extracts a substring from a string. The form is `(str-part string start)` or `(str-part string start length)`. Returns a new string starting at character index `start`, optionally limited to `length` characters. Returns an error if `start` is beyond the end of the string. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-part "Hello, World!" 7)
```


</td>
<td>

```clj
"World!"
```


</td>
</tr>
<tr>
<td>

```clj
(str-part "Hello, World!" 0 5)
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
(str-part "Hello, World!" 7 5)
```


</td>
<td>

```clj
"World"
```


</td>
</tr>
</table>




---


### str-join

`str-join` concatenates a list of strings into a single string. The form is `(str-join strings)` or `(str-join strings delimiter)` where `delimiter` is an optional string inserted between each element. The default delimiter is the empty string. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-join (list "Hello" " " "World"))
```


</td>
<td>

```clj
"Hello World"
```


</td>
</tr>
<tr>
<td>

```clj
(str-join (list "a" "b" "c") "-")
```


</td>
<td>

```clj
"a-b-c"
```


</td>
</tr>
<tr>
<td>

```clj
(str-join (list "one" "two" "three") ", ")
```


</td>
<td>

```clj
"one, two, three"
```


</td>
</tr>
</table>




---


### str-split

`str-split` splits a string into a list of substrings. The form is `(str-split string delimiter)` where `delimiter` is either a string or an integer. 

When `delimiter` is a string, the input is split at any character found in the delimiter string. 

When `delimiter` is an integer, the input is split into chunks of that many characters. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-split "hello world" " ")
```


</td>
<td>

```clj
("hello" "world")
```


</td>
</tr>
<tr>
<td>

```clj
(str-split "a,b;c,d" ",;")
```


</td>
<td>

```clj
("a" "b" "c" "d")
```


</td>
</tr>
<tr>
<td>

```clj
(str-split "abcdef" 2)
```


</td>
<td>

```clj
("ab" "cd" "ef")
```


</td>
</tr>
</table>




---


### str-replace

`str-replace` replaces all occurrences of a pattern in a string. The form is `(str-replace string pattern replacement)` or `(str-replace string pattern)` to remove all occurrences. Returns a new string with all occurrences of `pattern` replaced. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-replace "hello world" "world" "LispBM")
```


</td>
<td>

```clj
"hello LispBM"
```


</td>
</tr>
<tr>
<td>

```clj
(str-replace "aabbcc" "bb" "XX")
```


</td>
<td>

```clj
"aaXXcc"
```


</td>
</tr>
<tr>
<td>

```clj
(str-replace "hello world" "o")
```


</td>
<td>

```clj
"hell wrld"
```


</td>
</tr>
</table>




---


### str-replicate

`str-replicate` creates a string by repeating a character. The form is `(str-replicate n char)` where `n` is the number of characters and `char` is the character as a byte value. Returns a new string of length `n`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-replicate 5 97b)
```


</td>
<td>

```clj
"aaaaa"
```


</td>
</tr>
<tr>
<td>

```clj
(str-replicate 3 45b)
```


</td>
<td>

```clj
"---"
```


</td>
</tr>
<tr>
<td>

```clj
(str-replicate 8 120b)
```


</td>
<td>

```clj
"xxxxxxxx"
```


</td>
</tr>
</table>




---

## Searching and Comparing


### str-cmp

`str-cmp` compares two strings lexicographically. The form is `(str-cmp str1 str2)` or `(str-cmp str1 str2 n)` where `n` limits the comparison to the first `n` characters. Returns a negative integer if `str1` is less than `str2`, zero if they are equal, and a positive integer if `str1` is greater. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-cmp "abc" "abc")
```


</td>
<td>

```clj
0
```


</td>
</tr>
<tr>
<td>

```clj
(str-cmp "abc" "abd")
```


</td>
<td>

```clj
-1
```


</td>
</tr>
<tr>
<td>

```clj
(str-cmp "abd" "abc")
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(str-cmp "abcdef" "abcxyz" 3)
```


</td>
<td>

```clj
0
```


</td>
</tr>
</table>




---


### str-find

`str-find` searches for a substring within a string. The form is `(str-find string substr)` with optional additional arguments. Returns the index of the first match, or -1 if not found. The `substr` argument can also be a list of strings, in which case the search finds any of them. 

Optional arguments (may be given in any order after `substr`): 

   - An integer sets the start position for the search.
   - A second integer sets which occurrence to find (0 = first).
   - `'left` searches from right to left.
   - `'nocase` performs a case-insensitive search.

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-find "hello world" "world")
```


</td>
<td>

```clj
6
```


</td>
</tr>
<tr>
<td>

```clj
(str-find "hello world" "xyz")
```


</td>
<td>

```clj
-1
```


</td>
</tr>
<tr>
<td>

```clj
(str-find "abcabc" "bc" 0 1)
```


</td>
<td>

```clj
4
```


</td>
</tr>
<tr>
<td>

```clj
(str-find "Hello World" "world" 'nocase)
```


</td>
<td>

```clj
6
```


</td>
</tr>
<tr>
<td>

```clj
(str-find "abcabc" "bc" 'left)
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

## Case Conversion


### str-to-lower

`str-to-lower` converts a string to lowercase. The form is `(str-to-lower string)`. Returns a new string with all characters converted to lowercase. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-to-lower "Hello, World!")
```


</td>
<td>

```clj
"hello, world!"
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-lower "LISPBM")
```


</td>
<td>

```clj
"lispbm"
```


</td>
</tr>
</table>




---


### str-to-upper

`str-to-upper` converts a string to uppercase. The form is `(str-to-upper string)`. Returns a new string with all characters converted to uppercase. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str-to-upper "Hello, World!")
```


</td>
<td>

```clj
"HELLO, WORLD!"
```


</td>
</tr>
<tr>
<td>

```clj
(str-to-upper "lispbm")
```


</td>
<td>

```clj
"LISPBM"
```


</td>
</tr>
</table>




---

This document was generated by LispBM version 0.36.0 

