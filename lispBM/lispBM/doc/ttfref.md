# LispBM TTF Font Library

The LispBM TTF font extensions are based on the [libschrift](https://github.com/tomolt/libschrift) library 


# Reference


### ttf-prepare

`ttf-prepare` intializes font and prerenders glyphs. The result of `ttf-prepare` is a binary blob conaining all the information needed to print text using the prepared glyphs The form of a `ttf-prepare` expression is: `(ttf-prepare font-data scale img-format utf8-str)`. 

   - font-data : A ttf font file loaded or imported
   - scale : Floating point value specifying text size scaling.
   - img-format : Prerendering format. Formats are described in the [displayref](./displayref.md).
   - utf8-str : A string containing the UTF8 characters to prerender.

Note that only characters mentioned in the `utf-string` will be usable. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define b (ttf-prepare font 32 'indexed4 "helo wrd!"))
```


</td>
<td>

```clj
[0 0 0 0 102 111 110 116 0 108 109 116 120 0 0 0 0 12 65 238 151 142 192 193 137 56 63 101 96 66 107 101 114 110 0 0 0 0 96 0 0 0 4 0 0 0 104 0 0 0 1 0 0 0 119 190 212 253 244 0 0 0 0 0 0 0 111 0 0 0 1 0 0 0 119 190 212 253 244 0 0 0 0 0 0 0 114 0 0 0 1 0
```


</td>
</tr>
</table>

Note try to not put duplicate characters in the utf8-str. Duplicate characters use extra memory temporarily which could be a problem if you are already low on mem. 




---


### ttf-text

`ttf-text` draws text on an image from the [display library](./displayref.md). The form of an `ttf-text` expression is `(ttf-text img pos-x pos-y colors font-blob utf8-str opt-dir opt-linespacing)`. 

   - img : A display library image.
   - pos-x : Horizontal placement of text within the image.
   - pos-y : Vertical placement of text within the image.
   - colors : List of colors to use for antialiasing.
   - font-blob : The font object created by `ttf-prepare`.
   - utf8-str : The text to draw.
   - opt-dir : Optional argument specifying direction up or down using symbols `up` and `down`.
   - opt-linespacing : A binary number specifying a scaling of the linespacing.

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define aa-red '(0 4456448 10027008 16711680))
```


</td>
<td>

```clj
(0 4456448 10027008 16711680)
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ttf-text disp 70 120 aa-red b "hello world!")
```


</td>
<td>

<img src=./images/img1.png >

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
(ttf-text disp 20 40 aa-red b "hello" 'down)
```


</td>
<td>

<img src=./images/img2.png >

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
(ttf-text disp 50 90 aa-red b "hello" 'up)
```


</td>
<td>

<img src=./images/img3.png >

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
(ttf-text disp 70 90 aa-red b "hello\nworld!")
```


</td>
<td>

<img src=./images/img4.png >

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
(ttf-text disp 70 90 aa-red b "hello\nworld!" 2.0 )
```


</td>
<td>

<img src=./images/img5.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### ttf-line-height

Obtain line-height from a prepared font object. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ttf-line-height b)
```


</td>
<td>

```clj
36.768002f32
```


</td>
</tr>
</table>




---


### ttf-ascender

Obtain the ascender metrics from a prepared font object. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ttf-ascender b)
```


</td>
<td>

```clj
29.824001f32
```


</td>
</tr>
</table>




---


### ttf-descender

Obtain the descender metrics from a prepared font object. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ttf-descender b)
```


</td>
<td>

```clj
-6.048000f32
```


</td>
</tr>
</table>




---


### ttf-line-gap

Obtain the line-gap metrics from a prepared font object. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ttf-line-gap b)
```


</td>
<td>

```clj
0.896000f32
```


</td>
</tr>
</table>




---


### ttf-glyph-dims

Obtain the dimensions of a glyph from a prepared font object. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ttf-glyph-dims b "o")
```


</td>
<td>

```clj
(18u 20u)
```


</td>
</tr>
</table>




---


### ttf-text-dims

Obtain the dimensions of a string of text rendered using a prepared font object. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ttf-text-dims b "hello")
```


</td>
<td>

```clj
(72u 36u)
```


</td>
</tr>
</table>




---

# Examples


### Example: Using a font

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-clear disp)
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (import "Roboto-Regular.ttf" 'roboto)
 (define ft (ttf-prepare roboto 32 'indexed4 "helo wrd"))
 (define aa-green '(0 17408 39168 65280))
 (ttf-text disp 40 40 aa-green ft "hello world")
 (disp-render disp 0 0)
```


</td>
<td>

<img src=./images/img6.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>




---

This document was generated by LispBM version 0.30.3 

