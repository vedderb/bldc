# LispBM Array Extensions Reference Manual

The array extensions provide functions for reading and writing typed values into byte buffers, copying buffer contents, and freeing arrays. These extensions may or may not be present depending on the platform and configuration of LispBM. 

Byte arrays are created using `bufcreate`, which is part of core LispBM and documented in the LispBM reference manual. All index and length arguments are in bytes. Multi-byte operations default to big-endian byte order unless `'little-endian` is passed as an optional argument. 

## Buffer Utilities


### buflen

`buflen` returns the size of a byte array in bytes. The form of a `buflen` expression is `(buflen buf)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(buflen (bufcreate 10))
```


</td>
<td>

```clj
10
```


</td>
</tr>
<tr>
<td>

```clj
(buflen (bufcreate 0))
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


### bufclear

`bufclear` clears a byte array by setting bytes to a given value. The form is `(bufclear buf)` or with optional arguments `(bufclear buf byte start len)`. All optional arguments can be omitted from the right. The default fill byte is 0, the default start is 0, and the default length is the remainder of the array from start. Returns `t`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 4))
(bufset-u8 b 0 1)
(bufset-u8 b 1 2)
(bufset-u8 b 2 3)
(bufset-u8 b 3 4)
(bufclear b)
b
```


</td>
<td>


```clj
[0 0 0 0]
```


</td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 4))
(bufset-u8 b 0 1)
(bufset-u8 b 1 2)
(bufset-u8 b 2 3)
(bufset-u8 b 3 4)
(bufclear b 255 1 2)
b
```


</td>
<td>


```clj
[1 255 255 4]
```


</td>
</tr>
</table>




---


### bufcpy

`bufcpy` copies bytes from one byte array to another. The form is `(bufcpy dst dst-start src src-start len)`. Copies `len` bytes from `src` starting at `src-start` into `dst` starting at `dst-start`. The number of bytes copied is clamped to fit within both arrays. Returns `t`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define dst (bufcreate 4))
(define src (bufcreate 4))
(bufset-u8 src 0 10)
(bufset-u8 src 1 20)
(bufset-u8 src 2 30)
(bufset-u8 src 3 40)
(bufcpy dst 0 src 0 4)
dst
```


</td>
<td>


```clj
[10 20 30 40]
```


</td>
</tr>
</table>




---

## Writing to Buffers


### bufset-[X]

The `bufset` functions write a typed value into a byte array at a given byte index. The form is `(bufset-[X] buf index value)` or `(bufset-[X] buf index value 'little-endian)`. The available variants are: 

   - bufset-i8  — signed 8-bit integer
   - bufset-i16 — signed 16-bit integer
   - bufset-i32 — signed 32-bit integer
   - bufset-u8  — unsigned 8-bit integer
   - bufset-u16 — unsigned 16-bit integer
   - bufset-u24 — unsigned 24-bit integer
   - bufset-u32 — unsigned 32-bit integer
   - bufset-f32 — 32-bit float

Multi-byte variants default to big-endian byte order. Pass `'little-endian` as the optional fourth argument to write in little-endian order. Returns `t` on success. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 8))
(bufset-i16 b 0 -1000)
(bufset-u16 b 2 5000)
(bufset-f32 b 4 3.140000f32)
b
```


</td>
<td>


```clj
[252 24 19 136 64 72 245 195]
```


</td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 8))
(bufset-i16 b 0 -1000 'little-endian)
(bufset-u16 b 2 5000 'little-endian)
(bufset-f32 b 4 3.140000f32 'little-endian)
b
```


</td>
<td>


```clj
[24 252 136 19 195 245 72 64]
```


</td>
</tr>
</table>




---


### bufset-bit

`bufset-bit` sets a single bit in a byte array. The form is `(bufset-bit buf bit-pos value)`. The `bit-pos` argument is the absolute bit position in the array, where bit 0 is the least significant bit of byte 0, bit 8 is the least significant bit of byte 1, and so on. A truthy `value` sets the bit to 1; a falsy value sets it to 0. Returns `t`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 2))
(bufset-bit b 0 1)
(bufset-bit b 1 1)
(bufset-bit b 7 1)
(bufset-bit b 8 1)
b
```


</td>
<td>


```clj
[131 1]
```


</td>
</tr>
</table>




---

## Reading from Buffers


### bufget-[X]

The `bufget` functions read a typed value from a byte array at a given byte index. The form is `(bufget-[X] buf index)` or `(bufget-[X] buf index 'little-endian)`. The available variants are: 

   - bufget-i8  — signed 8-bit integer
   - bufget-i16 — signed 16-bit integer
   - bufget-i32 — signed 32-bit integer
   - bufget-u8  — unsigned 8-bit integer
   - bufget-u16 — unsigned 16-bit integer
   - bufget-u24 — unsigned 24-bit integer
   - bufget-u32 — unsigned 32-bit integer
   - bufget-f32 — 32-bit float

Multi-byte variants default to big-endian byte order. Pass `'little-endian` as the optional third argument to read in little-endian order. The byte order used when reading must match the byte order used when writing. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 4))
(bufset-u16 b 0 1000)
(bufset-u16 b 2 2000)
(bufget-u16 b 0)
```


</td>
<td>


```clj
1000
```


</td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 4))
(bufset-i16 b 0 -500 'little-endian)
(bufget-i16 b 0 'little-endian)
```


</td>
<td>


```clj
-500
```


</td>
</tr>
<tr>
<td>


```clj
(define b (bufcreate 4))
(bufset-f32 b 0 2.718000f32 'little-endian)
(bufget-f32 b 0 'little-endian)
```


</td>
<td>


```clj
2.718000f32
```


</td>
</tr>
</table>




---

## Memory Management


### free

`free` explicitly frees a byte array, returning its memory immediately rather than waiting for garbage collection. The form is `(free buf)`. The array must be a read-write (non-constant) byte array. Returns `t` on success or `nil` if the array could not be freed. After calling `free`, the array must not be accessed again. 

Use `free` with care. Accessing a freed array results in undefined behaviour. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(free (bufcreate 100))
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

This document was generated by LispBM version 0.36.0 

